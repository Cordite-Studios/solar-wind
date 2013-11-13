module Test.Time where

import Test.Tasty
import Test.Tasty.HUnit

-- 3rd party imports
import Control.Concurrent.STM

-- Base imports
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Timeout

-- This library imports
import Solar.Cast.Time
import Solar.Cast.Internal.Time
import Solar.Utility.Delta

time :: TestTree
time = testGroup "Solar.Cast.Time"
    [ testCase "Construct ticker" $ do
        tc <- mkNewTicker
        t <- readTVarIO tc
        -- And the clock should not automatically begin
        threadId t @?= Nothing

    , testCase "Stop non-started clock" $ do
        tc <- mkNewTicker
        stopClock tc
        t <- readTVarIO tc
        threadId t @?= Nothing
    , testCase "Start a non-started clock" $ do
        -- Should not throw an exception
        tc <- mkNewTicker
        startClock tc
        t <- readTVarIO tc
        threadId t @?= Nothing
        stopClock tc
    , testCase "Start and Stop a non-started clock" $ do
        -- Should not throw an exception
        tc <- mkNewTicker
        startClock tc
        t <- readTVarIO tc
        when (isJust $ threadId t) $ error "Thread did not register"
        stopClock tc
        t' <- readTVarIO tc
        threadId t' @?= Nothing
        
    , testCase "Start an already started clock" $ do
        -- Should not throw an exception.
        -- Should return.
        p <- timeout 10000 $ bracket
            mkNewTicker
            stopClock
            (\tc -> do
                startClock tc
                startClock tc)
        p @?= Just ()
    , testCase "Correct spacing between counts" $ do
        tc <- mkNewTicker
        let x = 100
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        t <- readTVarIO tc
        tq <- readTVarIO $ deltaTicker t
        let total = foldl (\a v -> a + remaining v) 0 tq
        total @?= fromInteger x
    ]