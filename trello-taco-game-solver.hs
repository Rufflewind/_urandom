{-# LANGUAGE DeriveGeneric #-}
-- | A simple demo that solves the Taco game from Trello by brute-force:
--   <https://trello.com/taco-game>
--
--   Tested on the Glasgow Haskell Compiler 7.8.3 with the following packages:
--
--   * bytestring-0.10.4.0
--   * containers-0.5.5.1
--   * hashable-1.2.2.0
--   * parallel-3.2.0.4
--   * unordered-containers-0.2.5.1
--
--   To build the program, run:
--
--   > ghc -O2 trello-taco-game-solver.hs
--
--   To build the documentation, run
--
--   > haddock -h -o trello-taco-game-solver-docs trello-taco-game-solver.hs
--
--   Note: The parallelized version is not really faster and could use some
--         optimization / restructuring.
module Main where
import Control.Concurrent (getNumCapabilities, forkIO, killThread,
                           newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Parallel (par, pseq)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Hashable (Hashable(hashWithSalt, hash))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty, mconcat), (<>))
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.HashSet as Set
import qualified Data.ByteString as ByteString

------------------------------------------------------------------------------
--
-- * Utility

-- | Use unordered sets instead of ordered sets: seems to be ~20% faster here.
type Set a = Set.HashSet a

-- | Convert a @'Maybe'@ into an @'Either'@, mapping @'Just'@ onto @'Right'@
--   and @'Nothing'@ onto @'Left'@ filled with the given value.
maybeToEither :: e                      -- ^ substitute value for @'Nothing'@
              -> Maybe a                -- ^ the value to be converted
              -> Either e a
maybeToEither e = fromMaybe (Left e) . fmap Right

-- | Divide a list into roughly equal groups.
divide :: [a]                           -- ^ input list
       -> Int                           -- ^ number of groups
       -> [[a]]
divide list n = go 0 list
  where (q, r) = length list `quotRem` n
        go i l | i == n    = []
               | otherwise = l1 : go (succ i) l2
          where (l1, l2) = splitAt (q + if i < r then 1 else 0) l

-- | Group adjacent elements in a list that are identical, keeping track of
--   the number of times they occur.
groupAdjacent :: (Eq a, Num n) => [a] -> [(a, n)]
groupAdjacent []       = []
groupAdjacent (z : zs) = go (z, 1) zs
  where go yn        []                   = [yn]
        go yn@(y, n) (x : xs) | x == y    = go (y, n + 1) xs
                              | otherwise = yn : go (x, 1) xs

-- | Perform all actions asynchronously and waits for all of them to complete.
--   If any one of them fails, the entire group is aborted.
waitForAll :: [IO (Either e a)] -> IO (Either e [a])
waitForAll actions = do
  mvar <- newEmptyMVar
  let acquire   = (`mapM` actions) $ \ action -> forkIO $ do
        result <- action
        result `seq` putMVar mvar result
      release   = mapM_ killThread
      wait xs 0 = return (Right xs)
      wait xs n = do
        result <- takeMVar mvar
        case result of
          Left  e -> return (Left e)
          Right x -> wait (x : xs) (pred n)
  bracket acquire release (return (wait [] (length actions)))

-- | Modify a string by substituting the given characters at the given locations.
--   /Note:/ Each position can only be substituted once: e.g.
--   @[(1, 0xff), (1, 0x20)]@ is not allowed.
modifyByteString :: [(Int, Word8)] -> ByteString -> ByteString
modifyByteString changes = ByteString.concat .
                           modify (0, sortBy (compare `on` fst) changes)
  where modify (_, [])            s = return s
        modify (j, (i, c) : rest) s = l : c' : modify (succ i, rest) s'
          where (l, r) = ByteString.splitAt (i - j) s
                c'     = ByteString.singleton c
                s'     = ByteString.tail r

------------------------------------------------------------------------------
--
-- * Generic solver

-- | The evolver defines the mechanics of the game.  The function receives the
--   current state of the game and returns either
--
--   * a @'Left'@: indicating that the game has been won; its value is
--     arbitrary and is returned later on, or
--
--   * a @'Right'@: indicating that the game is still in progress and contains
--     the possible moves as well as the states after the corresponding moves
--     have been performed.  If there are no more moves, the game is
--     considered lost.
--
type Evolver end move state = state -> Either end [(move, state)]

data SolverError = GaveUp               -- ^ solution may or may not exist
                                        --   but requires too many moves
                 | NoSolution           -- ^ game is unwinnable
                 deriving (Eq, Ord, Read, Show)

-- | Attempt to solve the game within the given maximum number of steps.
solve :: (Hashable state, Eq state) =>
         Evolver end move state         -- ^ game mechanics
      -> Int                            -- ^ maximum number of moves allowed
      -> state                          -- ^ initial state of game
      -> Either SolverError (end, [move])
solve evolve depth state = case result of
  Left (end, solution) -> return (end, reverse solution)
  Right ([], _) -> Left NoSolution
  Right _       -> Left GaveUp
  where result = explore evolve (succ depth) (return (mempty, state)) mempty

-- | Similar to @'solve'@ but uses parallelism.  The result may not deterministic
--   and is probably slower due to bad implementation.
psolve :: (Hashable state, Eq state) =>
          Evolver end move state      -- ^ game mechanics
       -> Int                         -- ^ maximum number moves allowed
       -> state                       -- ^ initial state of game
       -> IO (Either SolverError (end, [move]))
psolve evolve maxDepth state = do
  nprocs <- getNumCapabilities
  let go 0     _     _           = return (Left GaveUp)
      go depth nodes knownStates = do
        result <- fmap (fmap mconcat) . waitForAll $ do
          nodes' <- divide nodes nprocs
          return (return (explore evolve 1 nodes' knownStates))
        case result of
          Left (end, solution) -> return (return (end, reverse solution))
          Right (nodes', knownStates') -> do
            -- we are supposed to remove the duplicate states in nodes'
            -- but this ended up making things slower!
            let nodes'' = nodes'
            case nodes'' of
              [] -> return (Left NoSolution)
              _  -> go (pred depth) nodes'' knownStates'
  go maxDepth (return (mempty, state)) mempty

-- | A node in the search tree.  Contains the history of the moves (in
--   reversed order) as well as the state of the game.
type Node move state = ([move], state)

-- | Perform all the allowed moves for every node, up to the given number of
--   times.
explore :: (Hashable state, Eq state) =>
           Evolver end move state       -- ^ game mechanics
        -> Int                          -- ^ maximum number of moves allowed
        -> [Node move state]            -- ^ nodes to start at
        -> Set state                    -- ^ known states to avoid
        -> Either (end, [move]) ([Node move state], Set state)
explore _      0     nodes knownStates = return (nodes, knownStates)
explore evolve depth nodes knownStates = do
  (nodes', knownStates') <- stepAll evolve nodes knownStates
  case nodes' of
    [] -> return (nodes', knownStates')
    _  -> explore evolve (pred depth) nodes' knownStates'

-- | Perform all the allowed moves for every node.  Returns either the winning
--   moves or the potential new states together with the updated known states.
stepAll :: (Hashable state, Eq state) =>
           Evolver end move state       -- ^ game mechanics
        -> [Node move state]            -- ^ nodes to start at
        -> Set state                    -- ^ known states to avoid
        -> Either (end, [move]) ([Node move state], Set state)
stepAll evolve nodes knownStates = do
  nodes' <- mconcat `fmap` sequence (fmap (step evolve) nodes)
  return (filterKnownStates nodes' knownStates)

-- | Filter out the states that have already been examined and add new states
--   to the set of known states in the process.
--   (Note: seems to be a bottleneck.)
filterKnownStates :: (Hashable state, Eq state) =>
                     [Node move state]  -- ^ nodes to be filtered
                  -> Set state          -- ^ known states
                  -> ([Node move state], Set state)
filterKnownStates nodes' knownStates = foldl' f ([], knownStates) nodes'
  where f (ns, ss) n@(_, s) | Set.member s ss = (ns, ss)
                            | otherwise       = (n : ns, Set.insert s ss)

-- | Perform all the allowed moves for a given node, returning either the
--   winning moves or the potential new states.
step :: Evolver end move state          -- ^ game mechanics
     -> Node move state                 -- ^ start at this node
     -> Either (end, [move]) [Node move state]
step evolve (history, state) = case evolve state of
  Left  end   -> Left (end, history)
  Right dests -> Right $ do
    (move, state') <- dests
    return (move : history, state')

------------------------------------------------------------------------------
--
-- * Taco game

type Game = (Board, State)

-- | Environment of the game, which is fixed at the beginning of a game.
data Board = Board
             { boardWidth  :: !Int
             , boardHeight :: !Int
             } deriving (Eq, Read, Show)

-- | State of the game, which varies as the game progresses.
data State = State
             { playerPosition :: !Int        -- ^ Position of the player, encoded
                                             --   as @x + width * y@.
             , boardTiles     :: !ByteString -- ^ Tiles stored in row-major format.
             } deriving (Eq, Generic, Read, Show)

instance Hashable State

-- | Potential moves in the game.
data Move = MoveLeft
          | MoveRight
          | MoveUp
          | MoveDown
          deriving (Bounded, Enum, Eq, Read, Show)

-- | Status of the game: @'Just'@ means the game is in progress, @'Nothing'@
--   means the game has been won.  If the game is in-progress there are no
--   moves available, then the game is considered lost.
type Status = Maybe [(Move, State)]

-- | Convenient function for constructing games.
newGame :: Board -> (Int, Int) -> [Tile] -> (Board, State)
newGame b@(Board w _) (x, y) ts = (b, State (x + w * y) (ByteString.pack ts))

-- | Pretty-print the current state.
showState :: Board -> State -> String
showState (Board w h) (State pos ts')
  | ByteString.length ts' == w * h = go 0 0 (ByteString.unpack ts')
  | otherwise                      = "showState: invalid state"
  where go x y ts | y == h           = []
                  | x == w           = '\n' : go 0 (succ y) ts
                  | x + w * y == pos = '*'  : s !! 1 : go (succ x) y ts''
                  | otherwise        = s    <> go (succ x) y ts''
            where s       = showTile t
                  t : ts'' = ts

-- ** Mechanics

-- | Attempt to perform a move.  If the move is forbidden, @'Nothing'@ is
--   returned; otherwise, the new state is returned.
tryMove :: Board -> Move -> State -> Maybe State
tryMove (Board w h) m (State pos ts) = do

  -- This is the most complicated and critical part as it contains the main
  -- logic of the game.
  --
  -- The `Maybe` monad is convenient here since there are many different ways
  -- in which the move can fail.
  --
  -- The `changes` variable (also the primed version) contains a function that
  -- records what changes need to be made to the tiles.

  -- Alter the tile that the player is currently on.
  let tile     = ByteString.index ts pos
      changes  = case tileFloor tile of
        x | x == crackedFloor -> ((pos, emptyTile) :)
          | x == stoneFloor   -> ((pos, crackedFloor) :)
          | otherwise         -> id

  -- Inspect the tile to which the player moves, making sure that there's no
  -- obstruction or missing floor.
  nextPos     <- shift pos
  let nextTile = ByteString.index ts nextPos
  nextFloor   <- case tileFloor nextTile of
    x | x == emptyTile -> Nothing
      | otherwise      -> return x
  changes'    <- (. changes) `fmap` case tileObject nextTile of
    x | x == emptyTile -> return id
      | x == keyObject -> return ((nextPos, nextFloor) :)
      | x == redWall   -> (.     ((nextPos, nextFloor) :)) `fmap` do

        -- Inspect the tile just beyond the tile to which the player moves.
        -- This is needed to check whether there's another object
        -- preventing the red wall from being moved.
        nextNextPos <- shift nextPos
        let nextNextTile = ByteString.index ts nextNextPos in
          if tileObject nextNextTile == emptyTile
          then if tileFloor nextNextTile == emptyTile
               then return ((nextNextPos, redFloor) :)
               else return ((nextNextPos, nextNextTile .|. redWall) :)
          else Nothing

      | otherwise      -> Nothing
  return (State nextPos (modifyByteString (changes' []) ts))

  -- The `shift` function performs the move only if the destination lies
  -- within the board.
  where shift p = case m of
          MoveLeft  | x == 0     -> Nothing
                    | otherwise  -> return (p - 1)
          MoveRight | x == w - 1 -> Nothing
                    | otherwise  -> return (p + 1)
          MoveUp    | y == 0     -> Nothing
                    | otherwise  -> return (p - w)
          MoveDown  | y == h - 1 -> Nothing
                    | otherwise  -> return (p + w)
          where (y, x) = p `quotRem` w

-- | Perform a single move in every possible way.
evolveGame :: Board -> State -> Status
evolveGame env state@(State _ tiles) = do
  _ <- ByteString.find ((== keyObject) . tileObject) tiles
  return $ do
    move <- [minBound .. maxBound]
    let maybeState' = tryMove env move state
    case maybeState' of
      Nothing    -> []
      Just state' -> return (move, state')

-- ** Tiles

-- | Tiles are represented as an 8-bit unsigned integer.  The last two bits
--   are contain the type of floor, if any (cracked, stone, or red).  The next
--   two bits contain the type of object, if any (key, red, or blue).
type Tile = Word8

-- | Pretty-print the tile as a 2-character string.
showTile :: Tile -> String
showTile t = obj <> flr
  where obj = case tileObject t of
          x | x == blueWall  -> "@"
            | x == redWall   -> "O"
            | x == keyObject -> "$"
            | x == emptyTile -> " "
            | otherwise      -> "?"
        flr = case tileFloor t of
          x | x == crackedFloor -> "."
            | x == stoneFloor   -> ":"
            | x == redFloor     -> "o"
            | x == emptyTile    -> " "
            | otherwise         -> "?"

-- | Project out the bits of the floor component.
tileFloor  :: Tile -> Tile
tileFloor  = (0x3 .&.)

-- | Project out the bits of the object component.
tileObject :: Tile -> Tile
tileObject = (0xc .&.)

-- | Empty tile: all bits are zero.
emptyTile    :: Tile
emptyTile    = 0x0

-- | Cracked stone floor.
crackedFloor :: Tile
crackedFloor = 0x1

-- | Ordinary stone floor.
stoneFloor   :: Tile
stoneFloor   = 0x2

-- | Red floor, formed by a red wall falling into an empty tile.
redFloor     :: Tile
redFloor     = 0x3

-- | A key.  Every key must be acquired to win the game.
keyObject    :: Tile
keyObject    = 0x4

-- | A red wall, impedes movement but can be moved if there are no objects
--   behind it.
redWall      :: Tile
redWall      = 0x8

-- | A blue wall, which is immovable and impedes all movement.
blueWall     :: Tile
blueWall     = 0xc

------------------------------------------------------------------------------
--
-- * Example

-- | All 64 levels from: <https://trello.com/taco-game>
exampleGames :: [Game]
exampleGames =
  [ newGame (Board 3 3) (0, 1)
    [ bs, bs, bs
    ,  r,  r, kr
    ,  s, rs,  s ]
  , newGame (Board 4 3) (0, 0)
    [  c,  c,  s,  s
    ,  x,  x,  x,  s
    , kc,  c, kc,  c ]
  , newGame (Board 4 4) (0, 0)
    [  c, rc,  x, ks
    , rc,  x,  x, rc
    , bs,  x,  x,  x
    , kr,  r,  r, kc ]
  , newGame (Board 5 5) (4, 3)
    [ ks, kr,  c,  x, rr
    ,  r,  x, kc,  x, kr
    ,  s, rr,  x, kr, kc
    , rc, bs, rs, rc,  c
    ,  x,  x,  r,  x, kc ]
  , newGame (Board 5 5) (2, 0)
    [  s,  c,  c,  s, bs
    , kc, rs, kr, rr, bs
    , kr, rr, rr,  c, bs
    , bs, bs, kr,  x, ks
    ,  s, rc, ks,  x, ks
    ]
  , newGame (Board 5 5) (1, 4)
    [  s,  s,  x, kr,  s
    , kc,  c, kr, rc, rc
    , kr,  r, rs, kr, ks
    , rc, rc, rr,  r,  s
    , rs,  s, rr, bs, rr
    ]
  , newGame (Board 5 5) (0, 3)
    [ kr,  s,  x, ks,  c
    ,  s,  c,  x, kc, kc
    , rs, kc,  x,  r, rs
    ,  s, rc, kr, kr, rs
    ,  s,  c, rc,  s,  c ]
  , newGame (Board 5 5) (3, 4)
    [ rs,  r, kc,  c,  x
    , kc,  c, bs, kc, kr
    , rc, ks, kc, bs,  c
    ,  x, rs, kc, rc,  r
    ,  x,  s,  s,  c, rr ]
  , newGame (Board 5 5) (1, 3)
    [  r,  s, rc, kr, rr
    , rc, rr,  r, ks,  x
    ,  c, bs,  r,  x, kr
    ,  c,  s,  s,  s,  c
    , rs, rr, rc, rc, kc ]
  , newGame (Board 5 5) (4, 0)
    [ bs,  x, ks, kc,  r
    ,  x,  r, rc,  c, rc
    , kr, bs,  c,  x,  c
    ,  c, rr, ks,  x,  s
    , ks, rr,  x,  c,  s]
  , newGame (Board 5 5) (1, 4)
    [  s,  s,  x, kc,  c
    , rr, bs, bs,  x,  r
    , rc, rc, kc, kc,  s
    , bs, rc, rs,  r,  c
    , ks,  c, kr, kr, kc ]
  , newGame (Board 5 5) (0, 3)
    [ rs,  x,  s,  x, rr
    ,  s,  s, rs, rc, kr
    , kr,  c,  r,  c,  x
    ,  c,  r, rc, bs, kc
    , rs, kc, ks, rr,  s ]
  , newGame (Board 5 5) (1, 4)
    [  r,  c,  s,  s,  r
    ,  r, ks, rc, kr,  s
    , rc, rs, rr, kc, rs
    ,  c, kr, bs, rc, ks
    , rc,  c, kr, kc, bs ]
  , newGame (Board 5 5) (1, 3)
    [ ks, rr, bs, rc, rr
    , kr, bs, rc,  s,  c
    ,  c, kc, kr,  s, rr
    ,  x,  c, rr,  r,  x
    , ks,  r, rs, kc, rc ]
  , newGame (Board 5 5) (0, 2)
    [ ks, kr, rc,  r,  x
    , rc, ks, rs, kc, rr
    ,  c, kc, bs, rr, bs
    , rc,  s, rs,  r,  c
    ,  s, kr, bs, kc, rs ]
  , newGame (Board 5 5) (1, 3)
    [ rc,  s, ks,  x,  c
    , rc,  r,  c, kr, kc
    , ks, rs, rr, bs,  s
    , rs,  s, rc, ks, rr
    , kc,  x, rc, kr,  s ]
  , newGame (Board 5 5) (0, 2)
    [  r, ks,  s, rs,  s
    , kc, rc, rs, rr, rr
    ,  c, ks, bs,  c, kc
    ,  r, rr,  c,  c, rc
    ,  x,  r, kc,  x,  r ]
  , newGame (Board 5 5) (2, 4)
    [  c,  r,  s, kr, rc
    ,  r, rc, bs, rc, bs
    ,  r, kr, kc,  c,  x
    , ks,  x, bs, kr, kc
    ,  x, rc,  s, rc, ks ]
  , newGame (Board 5 5) (4, 2)
    [ kr, rr, kc, bs, rs
    ,  c,  s, rs, ks, rs
    , rc,  x, bs,  r,  r
    , rc, rs, ks,  x,  s
    ,  c, rc, ks, kc, kr ]
  , newGame (Board 5 5) (4, 2)
    [ kc, kc,  s, ks,  x
    ,  c, kc, bs,  c,  r
    , rc,  s,  r, rc,  r
    ,  s, rs, bs, kc, ks
    ,  x, kc,  c, rc,  x ]
  , newGame (Board 5 5) (4, 4)
    [  c, kc, rs,  x, bs
    ,  x, kr, rc, kr, rc
    , rr,  x, rc, rr,  x
    ,  c,  s,  s,  r, rc
    , kr, rc, rr, bs,  r ]
  , newGame (Board 5 5) (4, 0)
    [ kc,  x, ks,  r,  s
    , bs, kr, kc,  r, rs
    ,  x, rc,  r, ks, bs
    ,  s, rr, rs, rc, rc
    , bs, rr, kc,  x, bs ]
  , newGame (Board 5 5) (4, 4)
    [ kc, ks, kr,  x,  c
    , rr, rr,  c,  c,  s
    , rc,  r, bs, rr,  s
    , kc, rr,  x, kc, kc
    ,  c, rc, kc,  r,  s ]
  , newGame (Board 5 5) (4, 1)
    [  x, kc,  r,  c, rr
    , kr, rs, rr, kr,  c
    , rs, kr,  c,  c, bs
    ,  c, ks, rr, kc, bs
    , ks, rc,  r,  c, bs ]
  , newGame (Board 5 5) (3, 0)
    [ rs, kc, bs,  r,  c
    , rs,  c, kc,  c, bs
    , rs,  c,  c, ks, ks
    , kc,  c,  c,  s, ks
    ,  s, rc,  c, ks, rc ]
  , newGame (Board 5 5) (3, 0)
    [  x, rs, ks,  c, rr
    ,  c,  c, rc,  c,  c
    ,  r, kr,  c,  c,  x
    , kc,  x, rc, bs,  s
    ,  x,  c,  r, kc, bs ]
  , newGame (Board 5 5) (0, 1)
    [ rr, kr, kr, ks,  x
    ,  r,  c, bs, rc, ks
    , rs, rc,  c,  r, rc
    ,  c,  c,  r,  c, kr
    , kr, rr,  x, kr,  s ]
  , newGame (Board 5 5) (0, 0)
    [  c, rc,  x, kr, rs
    , rc, bs,  x, ks,  s
    , kc, rc, ks, kr, kc
    ,  r, rc,  c, rr,  r
    ,  s,  c,  s, bs, kc ]
  , newGame (Board 5 5) (0, 0)
    [  c, rs,  r,  x, rc
    , rr,  r,  x,  c,  r
    , rs,  s, kc, rc,  s
    , kc, kc,  r, bs, kr
    , rs, rr, rr,  c, kr ]
  , newGame (Board 5 5) (4, 3)
    [ rr,  c, ks, bs,  r
    , kr, ks, bs,  r, rr
    , rr, kc, bs, rc, ks
    ,  x,  c,  x,  c,  s
    , ks, bs, bs,  s,  c ]
  , newGame (Board 5 5) (4, 2)
    [  c, rr,  r, kc, rr
    , rc, kr, bs, ks,  x
    ,  r,  c, rr, kr,  c
    , kc,  x, bs, rr, kr
    ,  x,  c,  x, rs,  r ]
  , newGame (Board 5 5) (3, 1)
    [ ks,  c,  c,  r,  x
    ,  s, rs,  x,  c, rs
    , ks, rc, ks,  x,  c
    , rs, ks, bs,  c,  s
    ,  c,  x, bs, ks, kr ]
  , newGame (Board 5 5) (4, 2)
    [  s, kc, kc,  s, kr
    , kc, rs, bs, rr,  c
    ,  c, kc, rc, ks,  c
    ,  r, rs, rs, rr, kr
    ,  x, rc,  c,  x, rc ]
  , newGame (Board 5 5) (4, 0)
    [ ks, bs, kc, kr,  c
    , kr, rc, rs, kc, bs
    ,  x,  c,  r,  c, rr
    , rr, rc, rc, rc, kc
    ,  s,  s,  s,  s,  x ]
  , newGame (Board 5 5) (4, 1)
    [  s,  r, rc, ks,  s
    , kc, kc,  c, rs,  r
    , bs, rc, kr, rc, rs
    ,  r, rr,  x,  x, bs
    , rr, rr, rc,  r, ks ]
  , newGame (Board 5 5) (0, 0)
    [  c, kr,  c, rc, rc
    , ks, rs, kc, kc, rs
    ,  c, rr, kc, rr,  r
    , kc, rr, rc,  s, bs
    ,  c, rr, ks,  x,  r ]
  , newGame (Board 5 5) (3, 3)
    [ kc,  r, bs,  x, rr
    , rr, ks, kc,  x, rc
    ,  x, rc, kc,  s,  s
    ,  s, ks,  x,  c, rs
    , ks,  x,  r, ks,  s ]
  , newGame (Board 5 5) (4, 1)
    [ rr, kr, rs,  x, kr
    , rc,  r, bs, rc,  r
    , ks,  c, rr,  c, kc
    , rr, ks, kr,  c, bs
    , kr,  s, bs,  r, bs ]
  , newGame (Board 5 5) (3, 3)
    [ kc,  r,  x, rs,  s
    , rr, kc, bs, rc, ks
    , kc, rc, rs, kr,  c
    ,  c,  r, kr,  c,  r
    , bs, rc, bs, kc, bs ]
  , newGame (Board 5 5) (3, 0)
    [  r, kc,  s,  s,  x
    ,  r,  x, bs,  r, kc
    ,  c, kc, rs,  r, rc
    , kr,  c,  x, bs,  x
    ,  r, kc, rc,  c, ks ]
  , newGame (Board 5 5) (4, 3)
    [ ks, kr,  c,  c, rr
    , kc,  r,  x,  r,  r
    , kr, rs,  x,  c, rs
    , rr, rs,  s, kc,  c
    ,  c,  x, kr, rs, ks ]
  , newGame (Board 5 5) (4, 0)
    [ ks, rr, kc, rc,  s
    ,  r, kr, rc,  x, kr
    ,  r,  s,  r, rc, rc
    , kc, rc,  r,  x,  x
    ,  r, rc,  c, rs, rr ]
  , newGame (Board 5 5) (3, 4)
    [ ks, kc,  c, rc,  c
    , kr, ks, rc,  x, ks
    , rr,  r,  s, rc, rs
    , rr, rs, rs,  x,  x
    , kc,  c,  s,  c,  s ]
  , newGame (Board 5 5) (0, 4)
    [ kr,  x, rr, rc, ks
    ,  x, kr, kr, kr,  s
    , rr, rs,  r, rc, rc
    , rc,  r, kc, ks,  s
    ,  s, rs,  s,  s, rs ]
  , newGame (Board 5 5) (4, 0)
    [ rc, rs,  x, rr,  c
    , kr,  r,  s,  x, kc
    , rs, rc, bs, rr, rc
    ,  x,  s,  x,  x, kr
    ,  x, kr, kr, rr, rs ]
  , newGame (Board 5 5) (0, 4)
    [ kc,  x, rc, kc,  r
    , rs,  x, kc, rs, rc
    , kr, rc, rr,  x, kr
    , rc,  s, rc,  r,  x
    ,  c,  c,  s,  s,  r ]
  , newGame (Board 5 5) (2, 0)
    [  r,  r,  s, rs,  s
    , kr, rs, bs,  c, ks
    , rs, kc, ks, rs, kr
    ,  c, rs, bs, rr, kc
    , rc, rs, rs, bs, kr ]
  , newGame (Board 5 5) (0, 1)
    [  s,  s,  r, kc, ks
    ,  s, rc, bs,  r, rr
    , ks,  x, rc,  r, ks
    ,  x, rs, rc,  s,  x
    , rc, kr,  s, rr,  c ]
  , newGame (Board 5 5) (2, 1)
    [ kc, rs,  x,  r,  r
    ,  c,  x,  c, kr, rc
    ,  x,  r, kc, rc,  r
    , rc, rc,  s, rr,  r
    , rr, rr, ks, ks,  c ]
  , newGame (Board 5 5) (1, 4)
    [ rc,  r, rs,  x, kc
    , rs,  r,  c, bs, rc
    , kc, rc, ks,  s,  c
    ,  r, rc, rc,  s, rc
    , bs,  c, ks, kr,  c ]
  , newGame (Board 5 5) (0, 0)
    [  c, rc, kc, kr, rc
    , kr,  s,  s,  x, bs
    , rc, rs, rc,  r,  x
    , kc,  c, kc,  x, ks
    ,  s,  c,  s, kr, rc ]
  , newGame (Board 5 5) (1, 1)
    [ bs, kr, rr, ks,  r
    ,  s,  s, kc, rs,  c
    ,  s,  c, bs, bs,  r
    ,  c, rr, ks,  c, rc
    , kc,  s, bs, kc,  r ]
  , newGame (Board 5 5) (2, 2)
    [ kr,  c,  c,  x,  c
    ,  c,  c, rs, kc, bs
    , rs, kr,  c, rc, kc
    , kc, rc, rc, rr,  r
    ,  r,  s,  c, kr, ks ]
  , newGame (Board 5 5) (2, 4)
    [ kr, ks, rr, kc,  x
    ,  x, kc,  r, rc,  s
    , kc, rs,  x,  c, rr
    ,  s, rs,  s,  r,  c
    , rr,  c,  c, rr,  r ]
  , newGame (Board 5 5) (2, 1)
    [  r,  c, ks,  c,  c
    ,  c, rs,  c,  x, kc
    , rr, rc,  r,  x, rs
    , rr,  r,  s,  r, rs
    , rs, kr,  x,  r, kr ]
  , newGame (Board 5 5) (4, 1)
    [  s, rs,  s, kr, bs
    ,  c,  x,  x,  r,  r
    ,  c, bs, ks,  s, rc
    ,  r, kr,  c, kc, bs
    , ks, rc, rr, kr,  c ]
  , newGame (Board 5 5) (2, 3)
    [  r, kr,  s, kr,  c
    , rc, bs,  x,  r, bs
    , kc, kc, bs,  x, rr
    ,  r, rs,  c, rs,  c
    , ks, ks,  c,  c, kr ]
  , newGame (Board 5 5) (0, 4)
    [  r,  r, rs, bs, rc
    , kc, kc, rc, rc,  s
    , kr, rc, rr, ks, bs
    , ks, ks, rs,  c, rs
    ,  r,  c,  r,  c, kc ]
  , newGame (Board 5 5) (4, 3)
    [  x,  r, ks, rs, ks
    , rr, bs,  s, rs,  x
    , rc, kr, kc, bs, rs
    , rr,  c, rc,  c,  r
    ,  x, rr, rs, rr, kc ]
  , newGame (Board 5 5) (4, 3)
    [ ks,  s, rs, bs, ks
    , kr,  r,  s, rc,  r
    , rr, ks, ks, rc,  c
    ,  c, rr, bs, rc, ks
    ,  c, kc,  c,  s,  s ]
  , newGame (Board 5 5) (4, 3)
    [ ks,  x,  c,  c,  r
    , kc, bs, kc, rs, kc
    ,  s, bs,  s, rs,  s
    ,  x, kr,  r, rr,  r
    , kr, ks, rs, rs, rc ]
  , newGame (Board 5 5) (4, 2)
    [ rs,  r,  c, rs, kc
    , rc, rc, kc, kr, kc
    ,  r,  r, rc, rr,  c
    ,  r,  s, kr, rc,  r
    ,  r, kr, bs,  r, rr ]
  , newGame (Board 5 5) (0, 3)
    [ kr,  x, kc,  r, kc
    ,  x,  s,  s, rs, rs
    ,  x, kr, bs, rc,  r
    ,  s, rc,  c,  r, rr
    , rr,  r, kr, kc, kr ]
  , newGame (Board 5 5) (0, 4)
    [  x, kr, kr, kr,  x
    , kr,  x, kr,  x, kr
    , kr,  x, kr,  x, kr
    , kr,  x, kr, kr, kr
    ,  s, kr, kr, kr,  x ]
  ]
  where kr = keyObject .|. r
        rr = redWall   .|. r
        rs = redWall   .|. s
        ks = keyObject .|. s
        kc = keyObject .|. c
        rc = redWall   .|. c
        bs = blueWall  .|. s
        x  = emptyTile
        c  = crackedFloor
        s  = stoneFloor
        r  = redFloor

-- | Solve all the example games.
solveExampleGames :: Bool               -- ^ use parallelism?
                  -> IO ()
solveExampleGames parallel =
  (`mapM_` zip [1 :: Int ..] exampleGames) $ \ (i, (board, state)) -> do
    putStrLn ("Level " <> show i <> ":\n\n" <> showState board state)
    let evolve   = maybeToEither () . evolveGame board
        maxDepth = 32
    result <- if parallel
              then psolve evolve maxDepth state
              else return (solve evolve maxDepth state)
    case result of
      Left  err         -> printError err
      Right ((), moves) -> printMoves moves
    putStrLn ""
  where printError err   = putStr ("    " <> show err)
        printMoves moves = (`mapM_` moves') $ \ (move, count) -> do
          putStr ("    " <> show count <> "x " <> drop 4 (show move) <> "\n")
          where moves' :: [(Move, Int)]
                moves' = groupAdjacent moves

main :: IO ()
main = solveExampleGames False

-- * Unused

-- | Stores the hash as a lazy field.  (Apparently a pessimization in this
--   case, oh well.)
data Hashed a = Hashed Int {-# UNPACK #-} !a

instance Eq a => Eq (Hashed a) where
  Hashed hx x == Hashed hy y = hx == hy && x == y
  Hashed hx x /= Hashed hy y = hx /= hy || x /= y

instance Hashable a => Hashable (Hashed a) where
  hashWithSalt salt (Hashed h _) = hashWithSalt salt h
  hash              (Hashed h _) = h

hashed :: Hashable a => a -> Hashed a
hashed x = Hashed (hash x) x

-- | Like @'sequence'@ but tries to be parallel.
psequence :: Monad m => [m a] -> m [a]
psequence []       = return []
psequence (m : ms) = m `par` rest `pseq` do
  x  <- m
  xs <- rest
  return (x : xs)
  where rest = psequence ms
