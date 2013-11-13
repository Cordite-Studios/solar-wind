{-# LANGUAGE DeriveDataTypeable #-}
module Solar.Cast.Internal.Time where

import Data.Time
import Control.Concurrent.STM
import Solar.Utility.Delta
import Control.Concurrent
import Data.Typeable
import Control.Exception as E

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

data ClockException = ClockAlreadyRunning | ClockStopped | ClockChanged
    deriving (Show, Typeable)

instance Exception ClockException

-- Functions 

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
    | tickLength t /= toTick tv = upClock' time t' tv
    -- ↑ Sanity check so people can't change stuff ad hoc
    | not diffB = return r
    | otherwise = do
        writeTVar (tick tv) $ applyTick' t time
        return r
    where
        t' = t {tickLength = toTick tv}
        lastT = tickedLast t
        ti = tickLength t
        diff = diffUTCTime time lastT
        tid = ti - diff
        diffB = tid <= 0
        -- ↑ When this is a tick.
        r = if diffB then ti else tid

applyTick   :: ()
            => UTCTime
            -> DCTicker
            -> STM ()
applyTick time tk' = do
    let tk = content tk'
    k <- readTVar $ tick tk 
    writeTVar (tick tk) $ applyTick' k time

applyTick'  :: ()
            => Tick
            -> UTCTime
            -> Tick
applyTick' k time =
    k {ticks = totalticks + 1, tickedLast = time}
    where totalticks = ticks k

getNextLength   :: ()
                => TickerD
                -> NominalDiffTime
getNextLength d = case d of
    (x:_) -> remaining x
    _ -> 86400 -- 1 day
