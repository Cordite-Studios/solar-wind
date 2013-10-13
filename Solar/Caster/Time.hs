module Solar.Caster.Time where

import Control.Concurrent.STM
import Control.Concurrent
import Solar.Utility.Delta
import Data.Time
import Control.Monad

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
    a == b = (toTick a == toTick b)

instance Ord Ticker where
    compare a b = compare (toTick a) (toTick b)

type TickerD = Queue Ticker NominalDiffTime
type DCTicker = DeltaContainer Ticker NominalDiffTime

data TickingClock = TickingClock
    { deltaTicker :: TVar TickerD
    , lastTick    :: UTCTime
    }


incClock :: TVar TickingClock -> IO ()
incClock t = do
    time <- getCurrentTime
    atomically $ do
        c <- readTVar t
        let diff = diffUTCTime time (lastTick c)
        incClock' time diff t
        writeTVar t $ c {lastTick = time}

incClock' :: UTCTime -> NominalDiffTime -> TVar TickingClock -> STM ()
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



getNextLength :: TickerD -> NominalDiffTime
getNextLength d = case d of
    (x:_) -> toTick.content $ x
    _ -> 86400 -- 1 day


upClock :: UTCTime -> DCTicker -> STM (NominalDiffTime)
upClock time t' = do
    let td = content t'
        tt = tick td
    t <- readTVar tt
    upClock' time t td 
    
upClock' :: UTCTime -> Tick -> Ticker -> STM (NominalDiffTime)
upClock' time t tv
    | (tickLength t) /= (toTick tv) = upClock' time (t {tickLength = toTick tv}) tv
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



applyTick :: UTCTime -> DCTicker -> STM ()
applyTick time tk' = do
    let tk = content tk'
    k <- readTVar $ tick tk 
    let totalticks = ticks k 
        newk = k {ticks = totalticks + 1, tickedLast = time}
    writeTVar (tick tk) newk

getClockTicker :: TVar TickingClock -> NominalDiffTime -> STM (TVar Tick)
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
        tsv <- newTVar $ TickingClock qts time
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
    stepUntil d time 1.0000001 endtime
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