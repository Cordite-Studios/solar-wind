module Solar.Utility.Wait where

import System.Timeout
import Control.Concurrent
import Control.Concurrent.STM

type WaitOn = MVar ()

sleepOn :: ()
        => WaitOn -- ^ Waiting variable
        -> Int -- ^ Time to sleep in microseconds
        -> IO () -- ^ Blocking action
sleepOn w i = do
    tryTakeMVar w -- Clear the state
    timeout i $ takeMVar w
    return ()

-- | Does similar to compare and swap
sleepOnSTM  :: (Eq a)
            => Int -- ^ Time to sleep in microseconds
            -> a -- ^ Original
            -> TVar b -- ^ Some structure
            -> (TVar b -> STM a)
            -> IO ()
sleepOnSTM i original s f = do
    timeout i $ atomically $ do
        current <- f s
        check $ current /= original
        -- Only let the transaction pass through
        -- if things HAVE changed!
    return ()

