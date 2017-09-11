{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module TreeTraversals where

-- | A continuation.
type Cont r m a = a -> m r

emptyCont :: Applicative m => Cont r m r
emptyCont = pure

infixr 5 |>

(|>) :: (b -> Cont r m a -> m r) -> Cont r m a -> Cont r m b
(|>) = flip

goJump :: Functor m => a -> Cont r m a -> m r
goJump = flip ($)

------------------------------------------------------------------------------

infixr 5 :|>

data Stack f r a where
  (:|>) :: f a b -> Stack f r b -> Stack f r a
  Empty :: Stack f r r

data Jump f where
  Jump :: a -> f a -> Jump f

------------------------------------------------------------------------------

data Node x = Node x (Maybe (Node x)) (Maybe (Node x))

------------------------------------------------------------------------------

-- | Recursive implementation.
preorderR :: forall m x. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
preorderR f = preorder where

  preorder node =
    case node of
      Nothing ->
        pure ()
      Just (Node value left right) ->
        f value *>
        preorder left *>
        preorder right

-- | CPS implementation.
preorderC :: forall m x. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
preorderC f node0 = goJump () (preorder node0 |> emptyCont) where

  preorder node () cont =
    case node of
      Nothing ->
        goJump () cont
      Just (Node value left right) ->
        f value *>
        goJump () (preorder left |>
                   preorder right |>
                   cont)

data Preorder x a b where
  Preorder :: Maybe (Node x) -> Preorder x () ()

-- | Explicit stack implementation.
preorderS :: forall m x. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
preorderS f node0 = go (Jump () (Preorder node0 :|> Empty)) where

  go (Jump r Empty) = pure r
  go (Jump _ (Preorder node :|> stack)) =
    case node of
      Nothing ->
        go (Jump () stack)
      Just (Node value left right) ->
        f value *>
        go (Jump () (Preorder left :|>
                     Preorder right :|>
                     stack))

inorderR :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
inorderR f node0 = inorder node0 where

  inorder node =
    case node of
      Nothing ->
        pure ()
      Just (Node value left right) ->
        inorder left *>
        f value *>
        inorder right

inorderC :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
inorderC f node0 = goJump () (inorder1 node0 |> emptyCont) where

  inorder1 node () cont =
    case node of
      Nothing ->
        goJump () cont
      Just (Node value left right) ->
        goJump () (inorder1 left |>
                   inorder2 value right |>
                   cont)

  inorder2 value right () cont =
    f value *>
    goJump () (inorder1 right |> cont)

data Inorder x a b where
  Inorder1 :: Maybe (Node x) -> Inorder x () ()
  Inorder2 :: x -> Maybe (Node x) -> Inorder x () ()

inorderS :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
inorderS f node0 = go (Jump () (Inorder1 node0 :|> Empty)) where

  go (Jump r Empty) = pure r
  go (Jump _ (Inorder1 node :|> stack)) =
    case node of
      Nothing ->
        go (Jump () stack)
      Just (Node value left right) ->
        go (Jump () (Inorder1 left :|>
                     Inorder2 value right :|>
                     stack))
  go (Jump _ (Inorder2 value right :|> stack)) =
    f value *>
    go (Jump () (Inorder1 right :|> stack))

postorderR :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
postorderR f node0 = postorder node0 where

  postorder node =
    case node of
      Nothing ->
        pure ()
      Just (Node value left right) ->
        postorder left *>
        postorder right *>
        f value

postorderC :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
postorderC f node0 = goJump () (postorder1 node0 |> emptyCont) where

  postorder1 node () cont =
    case node of
      Nothing ->
        goJump () cont
      Just (Node value left right) ->
        goJump () (postorder1 left |>
                   postorder1 right |>
                   postorder2 value |>
                   cont)

  postorder2 value () cont =
    f value *>
    goJump () cont

data Postorder x a b where
  Postorder1 :: Maybe (Node x) -> Postorder x () ()
  Postorder2 :: x -> Postorder x () ()

{- There's actually multiple ways to do this, depending on how you partition the
operations. -}
postorderS :: forall x m. Applicative m => (x -> m ()) -> Maybe (Node x) -> m ()
postorderS f node0 = go (Jump () (Postorder1 node0 :|> Empty)) where

  go (Jump r Empty) = pure r
  go (Jump _ (Postorder1 node :|> stack)) =
    case node of
      Nothing ->
        go (Jump () stack)
      Just (Node value left right) ->
        go (Jump () (Postorder1 left :|>
                     Postorder1 right :|>
                     Postorder2 value :|>
                     stack))
  go (Jump _ (Postorder2 value :|> stack)) =
    f value *>
    go (Jump () stack)
