{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module TIPM where
import Data.Void
  ( Void
  , absurd -- :: Void -> a
  )

-- law of excluded middle ("TND")
excluded_middle :: Either a (a -> Void)
excluded_middle = excluded_middle -- just trust me!

session_5a :: Either a (a -> Void)
session_5a = excluded_middle

session_5b :: ((b -> Void) -> a -> Void) -> a -> b
session_5b f a =
  case excluded_middle of
    Left  b     -> b
    Right not_b -> absurd (f not_b a)

session_5c :: ((a -> Void) -> Void) -> a
session_5c f =
  case excluded_middle of
    Left  a     -> a
    Right not_a -> absurd (f not_a)

data Forall t where
  Forall :: (forall a . t a) -> Forall t

data Exists t where
  Exists :: t a -> Exists t

data TToAll t a where
  TToAll :: (t a -> Forall t) -> TToAll t a

data NotT t a where
  NotT :: (t a -> Void) -> NotT t a

session_6a :: forall t . Exists (TToAll t)
session_6a =
  case excluded_middle of
    Left (Exists (NotT not_some_t)) ->
      Exists (TToAll (\ some_tt -> absurd (not_some_t some_tt)))
    Right not_not_some_t ->
      Exists (TToAll (\ _ -> Forall (
        case excluded_middle of
          Left tx ->
            tx
          Right not_tx ->
            absurd (not_not_some_t (Exists (NotT not_tx)))
      )))

either_to_func :: Either (a -> Void) b -> (a -> b)
either_to_func (Left not_a) a = absurd (not_a a)
either_to_func (Right b)    _ = b
