import Data.Foldable
import Data.Maybe
import qualified Criterion.Main as C
import qualified Data.IntMap as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import IndexableList

main :: IO ()
main = do
  C.defaultMain
    [ do
      let n = 1000
      C.bgroup "1k"
        [ do
          let f = fromJust . indexList [n, n - 1 .. 0]
          C.bench "indexList"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        , do
          let f = fromJust . indexListC [n, n - 1 .. 0]
          C.bench "indexListC"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        , do
          let f = Seq.index (Seq.fromList [n, n - 1 .. 0])
          C.bench "seq"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        , do
          let f = (Map.fromList (zip [0 ..] [n, n - 1 .. 0]) Map.!)
          C.bench "intMap"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        , do
          let f = ([n, n - 1 .. 0] !!)
          C.bench "list"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        , do
          let f = (Vec.fromList [n, n - 1 .. 0] Vec.!)
          C.bench "vector"
            (C.whnf (foldl' (+) (0 :: Int) . (f <$>)) [0 .. n])
        ]
    ]
