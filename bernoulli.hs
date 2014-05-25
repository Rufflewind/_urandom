import Data.Foldable (foldl')

-- | Returns an infinite list containing Euler zigzag numbers (sequence
--   A000111, also known as up/down numbers).
--
--   The sequence is computed using the Seidel triangle method.
eulerZigzag :: Num a => [a]
eulerZigzag = generate [1]
  where combine  row x = seq x $ head row + x : row
        generate row   = head row : generate (foldl' combine [0] row)

-- | Returns an infinite list containing Bernoulli numbers (sequence A027641
--   divided by sequence A027642).  The convention used here sets the second
--   Bernoulli number to -1/2.
--
--   The sequence is computed using Euler zigzag numbers.
bernoulli :: Fractional a => [a]
bernoulli = 1 : -1 / 2 : generate eulerZigzag (-2) 1 1
  where ratio x y = realToFrac (x :: Integer) / realToFrac (y :: Integer)
        generate ~(_ : z : zs) i p q = b : 0 : generate zs i' p' q'
          where b  = ratio (z * i) (p' - q')
                i' = negate $ i + 2 * signum i
                p' = p * 4
                q' = q * 16
