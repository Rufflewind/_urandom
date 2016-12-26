-- A sketchy attempt at implementing a type-safe vector for numerical
-- calculations.  Be warned that this is a barely functional prototype for
-- experimenting with the type system.
--
-- There's several goals here actually:
--
-- One is to figure out how to use type-level tags to ensure the dimensions of
-- the vectors are consistent.  To draw a linear-algebraic analogy, the tag
-- "i" could be thought of describing what basis the vector is represented in.
-- Vectors represented in different bases can't be added together.
--
-- Another is to figure out an interface that would allow arbitrary lifted
-- operations between multiple vectors efficiently (without creating a bunch
-- of intermediate vectors).  Ideally something like:
--
--     f <$> vx <*> vy <*> vz ...
--
-- The (incomplete) Applicative implementation below comes close, but it's (1)
-- really ugly and (2) not general enough for N-ary operations.
--
-- Lastly, it would be nice to support both Storable and non-Storable vectors.
-- Sadly I couldn't find a sensible to make this work, since it would outright
-- violate parametricity, thus conflicting with the Applicative interface.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Vec where
import Data.Foldable
import Data.Proxy
import GHC.TypeLits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | [internal] A vector in "reduced" form.
data RawVec i a where
  RawVec :: V.Vector a -> RawVec i a
  RawVecPure :: a -> RawVec i a

-- | A vector tagged with length information @i@.  The tag @i@ has knowledge
-- of the length of the vector.  It may also contain additional information
-- about the domain of the vector's index space.
--
-- [internal] Data type needs to be abstract.
data Vec i a where
  VecRaw :: RawVec i a -> Vec i a
  VecAp :: Vec i (a -> b) -> Vec i a -> RawVec i b -> Vec i b

instance (Show a, Finite i) => Show (Vec i a) where
  showsPrec p (v :: Vec i a) = showsPrec p $ do
    case toRawVec v of
      RawVec xs -> xs
      RawVecPure x -> V.replicate (finiteLength (Proxy :: Proxy i)) x

-- | A vector with the length information hidden from sight.  It can be
-- revealed by pattern matching on the 'SomeVec'.
data SomeVec a where
  SomeVec :: Finite i => Vec i a -> SomeVec a

instance Show a => Show (SomeVec a) where
  showsPrec p (SomeVec v) = showsPrec p v

-- | All vectors tagged with @i@ must satisfy the constraint @Finite i@ in
-- order to be of any use.
class Finite i where
  finiteLength :: proxy i -> Int

instance KnownNat n => Finite n where
  finiteLength (_ :: proxy n) = fromIntegral (natVal (Proxy :: Proxy n))

fromList :: forall a . [a] -> SomeVec a
fromList xs = do
  let v = V.fromList xs
  case someNatVal (fromIntegral (V.length v)) of
    Nothing -> error "fromList: did not expect negative length"
    Just (SomeNat (_ :: Proxy n)) ->
      SomeVec (VecRaw (RawVec v) :: Vec n a)

instance Functor (Vec i) where
  f `fmap` xs = pure f <*> xs

-- | [internal]
toRawVec :: Vec i a -> RawVec i a
toRawVec (VecRaw r) = r
toRawVec (VecAp _ _ r) = r

-- instance Index i => Foldable (Series i) where
--   foldr z f (VecRaw xs) = foldr z f xs
--   length (Series xs) = length xs

instance Functor (RawVec i) where
  f `fmap` xs = pure f <*> xs

instance Applicative (RawVec i) where
  pure x = RawVecPure x
  RawVecPure f <*> RawVecPure x = RawVecPure (f x)
  RawVecPure f <*> RawVec xs = RawVec (f <$> xs)
  RawVec fs <*> RawVecPure x = RawVec (($ x) <$> fs)
  RawVec fs <*> RawVec xs = RawVec (V.zipWith ($) fs xs)

instance Applicative (Vec i) where
  pure x = VecRaw (pure x)
  vfs <*> vxs = VecAp vfs vxs $ do
    let rxs = toRawVec vxs
    case vfs of
      VecRaw rfs ->
        rfs <*> rxs
      VecAp (VecRaw rf1s) vx1s _ -> do
        let rx1s = toRawVec vx1s
        case rf1s of
          RawVec f1s ->
            case rx1s of
              RawVec x1s ->
                case rxs of
                  RawVec xs ->
                    RawVec (V.zipWith3 ($) f1s x1s xs)
                  RawVecPure x ->
                    RawVec (V.zipWith (\ f1 x1 -> f1 x1 x) f1s x1s)
              RawVecPure x1 ->
                case rxs of
                  RawVec xs ->
                    RawVec (V.zipWith ($ x1) f1s xs)
                  RawVecPure x ->
                    RawVec (V.map (\ f1 -> f1 x1 x) f1s)
          RawVecPure f1 ->
            case rx1s of
              RawVec x1s ->
                case rxs of
                  RawVec xs ->
                    RawVec (V.zipWith f1 x1s xs)
                  RawVecPure x ->
                    RawVec (V.map (\ x1 -> f1 x1 x) x1s)
              RawVecPure x1 ->
                case rxs of
                  RawVec xs ->
                    RawVec (V.map (f1 x1) xs)
                  RawVecPure x ->
                    RawVecPure (f1 x1 x)
      -- we could do this all day ...
      VecAp (VecAp _ _ rf1s) vx1s rfs ->
        toRawVec (VecAp (VecRaw rf1s) vx1s rfs <*> vxs)

-- Hmm we could write Vec i a = Mat i 1 a
-- but how to do the reverse?
data Mat i j a where
  Mat :: V.Vector a -> Mat i j a

-- fromLists :: forall a . [[a]] -> SomeVec a
-- fromLists xs = do


matmul :: forall i j k a . (Finite i, Finite j, Finite k, Num a) =>
          Mat i j a -> Mat j k a -> Mat i k a
matmul (Mat va) (Mat vb) = Mat vc
  where
    vc = V.create $ do
      let ni = finiteLength (Proxy :: Proxy i)
      let nj = finiteLength (Proxy :: Proxy j)
      let nk = finiteLength (Proxy :: Proxy k)
      c <- MV.new (ni * nk)
      for_ [0 .. ni] $ \ i -> do
        for_ [0 .. nj] $ \ j -> do
          for_ [0 .. nk] $ \ k -> do
            MV.write c (i * nk + k) (va V.! (i * nj + j) * vb V.! (j * nk + k))
      pure c
