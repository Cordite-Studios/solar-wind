module Solar.Caster.Utility.Time where

import Control.Concurrent.STM
import Solar.Utility.Delta
import Data.Time
import Control.Monad

data Tick = Tick
    { ticks :: Integer
    , tickedLast :: UTCTime
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
        let diff = diffUTCTime (lastTick c) time
            int = tickInterval c
            amt  = floor $ diff / int
        replicateM_ amt $ incOnce time (deltaTicker c) amt
        writeTVar t $ c {lastTick = time}

incOnce :: UTCTime -> TVar TickerD -> Int -> STM ()
incOnce time td int = do
    d <- readTVar td
    let (ticked, later) = zipTo d 1
    forM_ ticked $ applyTick time
    let updatedTicks = map up ticked
        newList = foldr insert later updatedTicks
    writeTVar td newList
    where
        up v = v { remaining = (floor $ (fromIntegral $ remaining v) / dint)}
        dint = (fromIntegral int)::Double

applyTick :: UTCTime -> DeltaContainer Ticker -> STM ()
applyTick time tk' = do
    let tk = content tk'
    k <- readTVar $ tick tk 
    let totalticks = ticks k 
        newk = k {ticks = totalticks + 1, tickedLast = time}
    writeTVar (tick tk) newk

