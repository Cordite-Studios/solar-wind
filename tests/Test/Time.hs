module Test.Time where

import Test.Tasty
import Test.Tasty.HUnit

-- 3rd party imports
import Control.Concurrent.STM
import Data.Time

-- Base imports
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Timeout
-- import Control.Concurrent

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
        when (isNothing $ threadId t) $ error "Thread did not register"
        stopClock tc
    , testCase "Start and Stop a non-started clock" $ do
        -- Should not throw an exception
        tc <- mkNewTicker
        startClock tc
        stopClock tc
        t' <- readTVarIO tc
        when (isJust $ threadId t') $ error "Thread did not leave"
        
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
    , testCase "Correct totalling" $ do
        tc <- mkNewTicker
        let x = 100
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        t <- readTVarIO tc
        tq <- readTVarIO $ deltaTicker t
        let total = foldl (\a v -> a + remaining v) 0 tq
        total @?= fromInteger x
    , testCase "Correct Spacing" $ do
        tc <- mkNewTicker
        let x = 10
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        t <- readTVarIO tc
        tq <- readTVarIO $ deltaTicker t
        let spaced = map (\v -> remaining v == 1) tq
        assertBool "All spaced by 1 second" $ and spaced
    , testCase "Correct Subtraction when half-ticking" $ do
        tc <- mkNewTicker
        let x = 10
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        t <- getCurrentTime
        r1:r2:_ <- atomically $ do
            incClock' t 0.5 tc
            c <- readTVar tc
            readTVar $ deltaTicker c
        remaining r1 @?= (0.5::NominalDiffTime)
        remaining r2 @?= (1::NominalDiffTime)
    , testCase "Correct behavior when over-ticking" $ do
        tc <- mkNewTicker
        let x = 10
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        t <- getCurrentTime
        r1:r2:r3:r4:_ <- atomically $ do
            incClock' t 1.5 tc
            c <- readTVar tc
            readTVar $ deltaTicker c
        -- 2 should only have 0.5 seconds left.
        remaining r2 @?= (0.5::NominalDiffTime)
        -- 1 was overticked, so we use a modulo.
        -- 1.5 modulo 1 is 0.5
        remaining r1 @?= (0.5::NominalDiffTime)
        -- 3-1.5 = 1.5. 1.5 - (0.5) - (0.5) = 0.5
        remaining r3 @?= (0.5::NominalDiffTime)
        -- 4 should be left alone.
        remaining r4 @?= (1::NominalDiffTime)
    , testCase "Perfect tick on first items (1)" $ do
        tc <- mkNewTicker
        t <- getCurrentTime
        let x = 10
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        r1:r2:_ <- atomically $ do
            incClock' t 1 tc
            c <- readTVar tc
            readTVar $ deltaTicker c
        (r1t, r2t) <- atomically $ do
            r1' <- readTVar $ tick $ content r1
            r2' <- readTVar $ tick $ content r2
            return (ticks r1', ticks r2')
        assertBool "Bad Resultant tick" $ and
            [ (remaining r1 == 0) /= (r1t == 1)
            , (remaining r2 == 1) /= (r2t == 1)
            ]
    , testCase "Perfect tick on first items (2)" $ do
        tc <- mkNewTicker
        t <- getCurrentTime
        let x = 10
        atomically $ forM_ [1..x] $ \d ->
            getClockTicker tc (fromInteger d)
        r1:r2:_ <- atomically $ do
            incClock' t 1 tc
            incClock' t 1 tc
            c <- readTVar tc
            readTVar $ deltaTicker c
        (r1t, r2t) <- atomically $ do
            r1' <- readTVar $ tick $ content r1
            r2' <- readTVar $ tick $ content r2
            return (ticks r1', ticks r2')
        assertBool "Bad Resultant tick" $ and
            [ (remaining r1 == 0) /= (r1t == 1)
            , (remaining r2 == 0) /= (r2t == 1)
            ]
    ]