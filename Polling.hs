{-# LANGUAGE CPP #-}

-- for implementing `modifyIORef'`
import qualified Data.IORef

-- for implementing `poll`
import Data.IORef (newIORef, readIORef)
import Control.Concurrent (threadDelay)

-- for the demonstration
import Control.Exception (bracketOnError)
import System.Timeout (timeout)
import qualified System.Process as Proc

-- | Strict version of `modifyIORef`.
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
--   `Just` rather than `Nothing`).  The period of the polling doubles with
--   each attempt until it reaches a maximum.
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
              modifyIORef' delayRef $ min maxDelay . (* 2)
              poll' delayRef

main :: IO ()
main = do
  let procInfo  = Proc.proc "sleep" ["5"]
      timeLimit = 2000000

  -- run a process and wait for it to finish (but with a time limit)
  _ <- let prepare   = do
             (_, _, _, p) <- Proc.createProcess procInfo
             putStrLn "process started."
             return p
           go      p = do
             -- wait for process to exit
             putStrLn "waiting for process..."
             exitCode <- poll 100000 (Proc.getProcessExitCode p)
             putStrLn ("process exited with: " ++ show exitCode)
           onError p = do
             -- time limit reached
             Proc.terminateProcess p
             putStrLn "process terminated because it took too long."
       in timeout timeLimit (bracketOnError prepare onError go)

  return ()
