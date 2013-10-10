module Solar.Caster.Time where

import Control.Concurrent.STM
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

type TickerD = Queue Ticker

data TickingClock = TickingClock
    { deltaTicker :: TVar TickerD
    , lastTick    :: UTCTime
    , tickInterval:: NominalDiffTime
    }


incClock :: TVar TickingClock -> IO ()
incClock t = do
    time <- getCurrentTime
    atomically $ do
        c <- readTVar t
        let diff = diffUTCTime time (lastTick c)
            int = tickInterval c
            amt = floor $ diff / int
        replicateM_ amt $ incOnce (time, int) $ deltaTicker c
        when (amt > 0) $ writeTVar t $ c {lastTick = time}


incOnce :: (UTCTime, NominalDiffTime) -> TVar TickerD -> STM ()
incOnce (time, int) td = do
    d <- readTVar td
    let (ticked, later) = zipTo d 1
    forM_ ticked $ applyTick time
    newTicks <- mapM up ticked
    let updatedTicks = zipWith f ticked newTicks
        newList = foldr insert later updatedTicks
    writeTVar td newList
    where
        up = upClock (time, int)
        f v r = v {remaining = r}

upClock :: (UTCTime, NominalDiffTime) -> DeltaContainer Ticker -> STM (Integer)
upClock (time, int) t' = do
    let td = content t'
        tt = tick td
    t <- readTVar tt
    upClock' (time, int) t td 
    
upClock' :: (UTCTime, NominalDiffTime) -> Tick -> Ticker -> STM (Integer)
upClock' (time, int) t tv
    | (tickLength t) /= (toTick tv) = upClock' (time, int) (t {tickLength = toTick tv}) tv
    -- ↑ Sanity check so people can't change stuff ad hoc
    | not diffB = return ratioi
    | otherwise = do
        writeTVar (tick tv) $ t {ticks = tticks+1, tickedLast = time}
        return ratioi
    where
        lastT = tickedLast t
        ti = tickLength t
        diff = diffUTCTime time lastT
        tid = ti - diff
        diffB = tid <= 0
        -- ↑ When this is a tick.
        ratio = if diffB then ti / int else tid / int
        ratioi = ceiling ratio
        tticks = ticks t



applyTick :: UTCTime -> DeltaContainer Ticker -> STM ()
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
        _ -> undefined
    where
        filt v = diff == (toTick.content $ v)



-- mkDemoTicker time i' = do
--     let i = fromInteger i'
--         t = Tick 0 time i
--     tv <- newTVar t
--     return $ DeltaContainer (Ticker tv i) i'
-- mkDemo = do
--     time <- getCurrentTime
--     atomically $ do
--         ts <- mapM (mkDemoTicker time) [1..10]
--         qts <- newTVar ts
--         tsv <- newTVar $ TickingClock qts time 1
--         return tsv
-- 
-- showDemo tc' = do
--     putStrLn "Current List:"
--     ts <- atomically $ do
--         tc <- readTVar tc'
--         t <- readTVar $ deltaTicker tc
--         forM t $ \tk -> readTVar $ tick.content $ tk
--     forM_ ts $ \t -> do
--         putStrLn $ show t
