module Utils where

-- | Make an @'IO'@ action more strict by forcing the result with a given function.
mseq :: Monad m => (a -> b) -> m a -> m a
mseq force m = do
  x <- m
  force x `seq` return x

-- | Turns a @'Maybe'@ into a @'Bool'@.
maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing  = False
