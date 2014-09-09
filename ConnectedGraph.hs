import Data.Set (Set)
import qualified Data.Set as Set

findGroup :: Ord a
          => (a -> a -> Bool) -- ^ Predicate to check if nodes are connected
          -> Set a            -- ^ All nodes
          -> a                -- ^ Starting node
          -> (Set a, Set a)   -- ^ (nodes in group, remaining nodes)
findGroup connected nodes = find (Set.empty, nodes)
  where find (group, others) node = Set.foldl' find (group', others') new
          where group'            = Set.union group new
                (new, others')    = Set.partition (connected node) others

-- findConnected :: (Int -> Int -> Bool) -> Int -> ([Int], [Set Int])
-- left as an exercise for the reader >:)
