{-# LANGUAGE RankNTypes #-}
module IndexableList where
import Data.Bits (countLeadingZeros, finiteBitSize, testBit)

-- | A simple binary tree.
data Tree a = Node (Tree a) a (Tree a) | Leaf
  deriving (Eq, Ord, Read, Show)

type CTree r a = (r -> a -> r -> r) -> r -> r

leaf :: CTree r a
leaf _ z = z

node :: CTree r a -> a -> CTree r a -> CTree r a
node tl x tr f z = f (tl f z) x (tr f z)

-- | Build a tree from elements of the list in breadth-first order.
bfsTreeFromList :: [a] -> Tree a
bfsTreeFromList xs0 = n
  where
    (n, xssAll) = build (xs0 : xssAll)
    build xss =
      case xss of
        (x : xs) : xss0 -> (Node tl x tr, xs : xss2)
          where
            (tl, xss1) = build xss0
            (tr, xss2) = build xss1
        _ -> (Leaf, xss)

bfsCTreeFromList :: [a] -> CTree r a
bfsCTreeFromList xs0 = n
  where
    (n, xssAll) = build (xs0 : xssAll)
    build xss =
      case xss of
        (x : xs) : xss0 -> (node tl x tr, xs : xss2)
          where
            (tl, xss1) = build xss0
            (tr, xss2) = build xss1
        _ -> (leaf, xss)

-- | Find the value stored in the i-th node of a tree in breadth-first order.
lookupBfsTree :: Tree a -> Int -> Maybe a
lookupBfsTree root i
  | i < 0     = Nothing
  | otherwise = search root logJ
  where
    logJ = finiteBitSize j - 1 - countLeadingZeros j
    j = i + 1
    search t m =
      case t of
        Leaf             -> Nothing
        Node tl x tr
          | m == 0       -> Just x
          | testBit j m' -> search tr m'
          | otherwise    -> search tl m'
      where
        m' = m - 1

lookupBfsCTree :: CTree (Int -> Maybe a) a -> Int -> Maybe a
lookupBfsCTree root i
  | i < 0     = Nothing
  | otherwise = root search (const Nothing) logJ
  where
    logJ = finiteBitSize j - 1 - countLeadingZeros j
    j = i + 1
    search tl x tr m
      | m == 0       = Just x
      | testBit j m' = tr m'
      | otherwise    = tl m'
      where
        m' = m - 1

-- | Returns a function that can efficiently (amortized) yield the i-th
-- element of a list.  The function is lazy enough to support infinite lists.
indexList :: [a] -> Int -> Maybe a
indexList xs = lookupBfsTree (bfsTreeFromList xs)

indexListC :: [a] -> Int -> Maybe a
indexListC xs = lookupBfsCTree (bfsCTreeFromList xs)
