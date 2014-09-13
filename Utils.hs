{-# LANGUAGE CPP #-}
module Utils where
import qualified Data.IORef
import Data.IORef (newIORef, readIORef)
import Control.Concurrent (threadDelay)

-- | Make a monadic action more strict by forcing the result with a given function.
mseq :: Monad m => (a -> b) -> m a -> m a
mseq force m = do
  x <- m
  force x `seq` return x

-- | Turns a @'Maybe'@ into a @'Bool'@.
maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing  = False

-- | Strict version of @'modifyIORef'@.
modifyIORef' :: Data.IORef.IORef a -> (a -> a) -> IO ()
#ifndef MIN_VERSION_base
#  define MIN_VERSION_base(x, y, z) 0
#endif
#if MIN_VERSION_base(4, 6, 0)
modifyIORef' = Data.IORef.modifyIORef'
#else
modifyIORef' r f = do
  x <- Data.IORef.readIORef r
  let x' = f x in x' `seq` Data.IORef.writeIORef r x'
#endif

-- | Perform a given action periodically until it succeeds (i.e. returns
--   @'Just'@ rather than @'Nothing'@).  The period of the polling doubles
--   with each attempt until it reaches a maximum.
poll :: Int                             -- ^ Maximum polling period.
     -> IO (Maybe a)                    -- ^ An action to be performed.
     -> IO a
poll maxDelay query = newIORef 1 >>= poll'
  where poll' delayRef = do
          maybeResult <- query
          case maybeResult of
            Just result -> return result
            Nothing     -> do
              delay <- readIORef delayRef
              threadDelay delay
              modifyIORef' delayRef (min maxDelay . (2 *))
              poll' delayRef
