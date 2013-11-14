{-# LANGUAGE CPP #-}
module Solar.Cast.Time
    (   -- * Structures
      Tick(..)
    , Ticker(..)
    , TickerD
    , TickingClock
    , ClockException(..)
      -- * Main interaction points
    , mkNewTicker
    , getClockTicker
    , stopClock
    , startClock
      -- * Fine control
    , runClock
    , incClock
    , incClock'
    , mkNewTicker'
    )
where

import Solar.Cast.Internal.Time

import Control.Concurrent.STM
import Solar.Utility.Delta
import Solar.Utility.Wait
import Data.Time
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Data.Maybe(isNothing)

baseSleep :: Int
baseSleep = 100000

mkNewTicker' :: UTCTime -> STM (TVar TickingClock)
mkNewTicker' time = do
    q <- newTVar []
    newTVar $ TickingClock q time Nothing

mkNewTicker :: IO (TVar TickingClock)
mkNewTicker = do
    t <- getCurrentTime
    atomically $ mkNewTicker' t

stopClock :: TVar TickingClock -> IO ()
stopClock tc = do
    tid <- atomically $ f tc
    case tid of
        Nothing -> return ()
        Just t -> do
            throwTo t ClockStopped
            sleepOnSTM baseSleep tid tc f
    where
        f t = do
            c <- readTVar t
            return $ threadId c

startClock :: TVar TickingClock -> IO ()
startClock tc = do
    temp <- newTVarIO Nothing
    _ <- forkIOWithUnmask $ \unmask -> do
        tid <- myThreadId
        E.catch (unmask $ bracket
            (return ())
            (\_ -> atomically $ do
                c <- readTVar tc
                when (threadId c == Just tid) 
                    (writeTVar tc $ c { threadId = Nothing }))
            (\_ -> do
                atomically $ writeTVar temp $ Just ()
                runClock tc
                )) f
    -- Wait until the thread confirms that it has started.
    startSuccess <- sleepOnSTM' baseSleep Nothing temp readTVar
    when (not startSuccess) $ throw ClockDidNotStart
    where
        f :: ClockException -> IO ()
        f _ = return ()


incClock    :: ()
            => TVar TickingClock
            -> IO ()
incClock t = do
    time <- getCurrentTime
    atomically $ do
        c <- readTVar t
        let diff = diffUTCTime time (lastTick c)
        incClock' time diff t
        writeTVar t $ c {lastTick = time}

incClock'   :: ()
            => UTCTime
            -> NominalDiffTime
            -> TVar TickingClock
            -> STM ()
incClock' time dr t = do
    c <- readTVar t
    dt <- readTVar $ deltaTicker c
    unless (null dt) $ do
        -- We definitely have something to deduct from
        let (ticked, later) = zipTo dt dr
        forM_ ticked $ applyTick time
        newTicks <- mapM up ticked
        let updatedTicks = zipWith f ticked newTicks
            newList = foldr insert later updatedTicks
        writeTVar (deltaTicker c) newList
    where
        up = upClock time
        f v r = v {remaining = r}

getClockTicker  :: ()
                => TVar TickingClock
                -> NominalDiffTime
                -> STM (TVar Tick)
getClockTicker tc diff = do
    c <- readTVar tc
    td <- readTVar $ deltaTicker c
    case filter filt td of
        (x:_) -> return $ tick.content $ x
        _ -> do
            tv <- newTVar $ Tick 0 (lastTick c) diff
            let dc = DeltaContainer (Ticker tv diff) diff
                newq = insert dc td
            writeTVar (deltaTicker c) newq
            return tv
    where
        filt v = diff == (toTick.content $ v)

runClock' :: TVar TickingClock -> IO ()
runClock' tc = do
    mth <- myThreadId
    (cth, tq) <- atomically $ do
        c <- readTVar tc
        let tid = threadId c
        tq <- readTVar $ deltaTicker c
        return (tid, tq)
    unless (Just mth == cth) $ throw ClockChanged
    -- Get time to sleep
    dt <- atomically $ do
        c <- readTVar tc
        readTVar $ deltaTicker c
    let l = toRational (getNextLength dt) * 1000000
        fl = floor l
        fl :: Integer
        lf = fromIntegral fl
    -- putStrLn $ "Sleeping " ++ (show lf)
    -- Do the sleeping
    sleepOnSTM lf tq tc $ \ticking -> do
        c <- readTVar ticking
        readTVar $ deltaTicker c
    -- Hit the clock
    incClock tc
    -- Run again!
    runClock' tc

runClock    :: ()
            => TVar TickingClock
            -> IO ()
runClock tc = do
    -- Take ownership
    mth <- myThreadId
    gotOwnership <- atomically $ do
        c <- readTVar tc
        when (isNothing $ threadId c)
            (writeTVar tc $ c { threadId = Just mth})
        return $ isNothing $ threadId c
    if gotOwnership 
        then runClock' tc
        else throw ClockAlreadyRunning
