{-# LANGUAGE GADTs, RankNTypes #-}
module Quantification where
import Data.Profunctor (Star(Star))

data Exists f where
  Exists :: f a -> Exists f

existsToForall :: (Exists f -> b) -> forall a. f a -> b
existsToForall f x = f (Exists x)

forallToExists :: (forall a. f a -> b) -> Exists f -> b
forallToExists f (Exists x) = f x

shrinkForall :: (forall b. a -> f b) -> a -> (forall b. f b)
shrinkForall = ($)

expandForall :: (a -> (forall b. f b)) -> forall b. a -> f b
expandForall = ($)

shrinkExists :: Exists (Star f a) -> a -> Exists f
shrinkExists (Exists (Star f)) x = Exists (f x)

-- can't expandExists in intuitionistic logic!
-- expandExists :: (a -> Exists f) -> Exists (Star f a)
