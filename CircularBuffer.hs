-- | Circular buffer used to reassemble a data stream.
--
-- WARNING: The current design is flawed.  If the buffer becomes full,
--          it may become impossible to recover if there are gaps!
--
module CircularBuffer where
import Control.Arrow (first)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewL((:<)), (|>))
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B

-- | A circular buffer used to reassemble the data stream.
data Buffer w a =
  Buffer
  { b_buffer :: Seq (Maybe a)
  , b_front :: w
  , b_size :: Int
  , b_weigh :: a -> Int
  , b_maxSize :: Int
  }

newBuffer :: (Bounded w, Integral w) =>
             (a -> Int)            -- ^ function to compute size of an element
          -> Int                   -- ^ maximum size
          -> Buffer w a
newBuffer = Buffer Seq.empty 0 0

newBufferB :: (Bounded w, Integral w) =>
              Int                   -- ^ maximum size
           -> Buffer w ByteString
newBufferB = newBuffer (\ x -> B.length x + overhead)
  where overhead = 128

-- | Add an element to the buffer at the correct location.
--   If the element is located before the front, it is ignored.
--   If the element would cause the buffer to exceed its maximum size,
--   'Nothing' is returned.
addToBuffer :: (Bounded w, Integral w) =>
               w -> a -> Buffer w a -> Maybe (Buffer w a)
addToBuffer i x b@(Buffer buffer front size weigh maxSize)
  | ni < nfront     = Just b
  | size' > maxSize = Nothing
  | ni < nback      = Just (b{b_buffer=buffer1, b_size=size'})
  | otherwise       = Just (b{b_buffer=buffer2, b_size=size'})
  where
    nfront  = (minBound + maxBound) `div` 2
    ni      = nfront + d
    nback   = nfront + len
    d       = i - front
    len     = fromIntegral (Seq.length buffer)
    x'      = Just x
    buffer1 = Seq.update (fromIntegral d) x' buffer
    buffer2 = buffer <> Seq.replicate (fromIntegral (d - len)) Nothing |> x'
    size'   = size + weigh x

-- | Pull one element from the front of the buffer.
--   If there is no element available, nothing happens.
popFromBuffer :: (Bounded w, Integral w) => Buffer w a -> Maybe (a, Buffer w a)
popFromBuffer b@(Buffer buffer front size weigh _) =
  case Seq.viewl buffer of
    Just x :< buffer' ->
      Just ( x
           , b{ b_buffer = buffer'
              , b_front  = front + 1
              , b_size   = size - weigh x } )
    _ -> Nothing

-- | Pull as many as elements as possible from the front of the buffer.
pullFromBuffer :: (Bounded w, Integral w) => Buffer w a -> ([a], Buffer w a)
pullFromBuffer b@(Buffer buffer front size weigh _) =
  case Seq.viewl buffer of
    Just x :< buffer' ->
      first (x :) $
        pullFromBuffer
          b{ b_buffer = buffer'
           , b_front  = front + 1
           , b_size   = size - weigh x
           }
    _ ->
      ([], b)
