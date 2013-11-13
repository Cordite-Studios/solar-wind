{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Solar.Cast.Time
    (   -- * Structures
      Tick(..)
    , Ticker(..)
    , TickerD
    , TickingClock
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

import Control.Concurrent.STM
import Solar.Utility.Delta
import Solar.Utility.Wait
import Data.Time
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Data.Typeable
import Data.Maybe(isNothing)

data Tick = Tick
    { ticks :: Integer
    , tickedLast :: UTCTime
    , tickLength :: NominalDiffTime
    } deriving (Eq, Ord, Show)
data Ticker = Ticker
    { tick :: TVar Tick
    , toTick :: NominalDiffTime
    }
instance Eq Ticker where
    a == b = toTick a == toTick b

instance Ord Ticker where
    compare a b = compare (toTick a) (toTick b)

type TickerD = Queue Ticker NominalDiffTime
type DCTicker = DeltaContainer Ticker NominalDiffTime

-- | The clock structure which is passed around
-- and maintains information on the ownership
data TickingClock = TickingClock
    { deltaTicker :: TVar TickerD
    , lastTick    :: UTCTime
    , threadId    :: Maybe ThreadId
    }

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
    tid <- atomically $ do
        c <- readTVar tc
        return $ threadId c
    case tid of
        Nothing -> return ()
        Just t -> do
            throwTo t ClockStopped

startClock :: TVar TickingClock -> IO ()
startClock tc = do
    forkIOWithUnmask $ \unmask -> do
        tid <- myThreadId
        E.catch (unmask $ bracket
            (return ())
            (\_ -> atomically $ do
                c <- readTVar tc
                when (threadId c == Just tid) 
                    (writeTVar tc $ c { threadId = Nothing })
            )
            (\_ -> runClock tc)) f
    return ()
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

upClock     :: ()
            => UTCTime 
            -> DCTicker 
            -> STM NominalDiffTime
upClock time t' = do
    let td = content t'
        tt = tick td
    t <- readTVar tt
    upClock' time t td 
    
upClock'    :: ()
            => UTCTime
            -> Tick
            -> Ticker
            -> STM NominalDiffTime
upClock' time t tv
    | tickLength t /= toTick tv = upClock' time (t {tickLength = toTick tv}) tv
    -- ↑ Sanity check so people can't change stuff ad hoc
    | not diffB = return r
    | otherwise = do
        writeTVar (tick tv) $ t {ticks = tticks+1, tickedLast = time}
        return r
    where
        lastT = tickedLast t
        ti = tickLength t
        diff = diffUTCTime time lastT
        tid = ti - diff
        diffB = tid <= 0
        -- ↑ When this is a tick.
        r = if diffB then ti else tid
        tticks = ticks t

applyTick   :: ()
            => UTCTime
            -> DCTicker
            -> STM ()
applyTick time tk' = do
    let tk = content tk'
    k <- readTVar $ tick tk 
    let totalticks = ticks k 
        newk = k {ticks = totalticks + 1, tickedLast = time}
    writeTVar (tick tk) newk

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

getNextLength   :: ()
                => TickerD
                -> NominalDiffTime
getNextLength d = case d of
    (x:_) -> remaining x
    _ -> 86400 -- 1 day

runClock    :: ()
            => TVar TickingClock
            -> IO ()
runClock tc = do
    -- Sanity Check
    cth <- atomically $ do
        c <- readTVar tc
        return $ threadId c
    unless (isNothing cth) $ throw ClockAlreadyRunning
    -- Take ownership
    mth <- myThreadId
    atomically $ do
        c <- readTVar tc
        writeTVar tc $ c { threadId = Just mth}
    runClock' tc

runClock' :: TVar TickingClock -> IO ()
runClock' tc = do
    mth <- myThreadId
    (cth, q, tq) <- atomically $ do
        c <- readTVar tc
        let tid = threadId c
            dt = deltaTicker c
        cq <- readTVar dt
        tq <- readTVar $ deltaTicker c
        return (tid, dt, tq)
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


data ClockException = ClockAlreadyRunning | ClockStopped | ClockChanged
    deriving (Show, Typeable)

instance Exception ClockException


-- import Control.Concurrent
#ifdef TESTPLAY
mkDemoTicker time i' = do
    let i = fromInteger i'
        t = Tick 0 time i
    tv <- newTVar t
    return $ DeltaContainer (Ticker tv i) i
mkDemo = do
    time <- getCurrentTime
    atomically $ do
        ts <- mapM (mkDemoTicker time) [1..10]
        qts <- newTVar $ foldr insert [] ts
        tsv <- newTVar $ TickingClock qts time Nothing
        return tsv

showDemo tc' = do
    putStrLn "Current List:"
    ts <- atomically $ do
        tc <- readTVar tc'
        t <- readTVar $ deltaTicker tc
        forM t $ \tk -> do
            v <- readTVar $ tick.content $ tk
            return (v, remaining tk)
    forM_ ts $ \t -> do
        putStrLn $ show t

doDemo x = do
    d <- mkDemo
    showDemo d
    time <- getCurrentTime
    let endtime = addUTCTime (fromInteger x) time
    stepUntil d time 1.0 endtime
doDemo' x = do
    d <- mkDemo
    showDemo d
    replicateM_ x $ do
        threadDelay 1000000
        incClock d
        showDemo d


stepUntil d time diff endtime = do
    atomically $ incClock' ntime diff d
    showDemo d
    when (ntime < endtime) $ stepUntil d ntime diff endtime
    where ntime = addUTCTime diff time

#endif