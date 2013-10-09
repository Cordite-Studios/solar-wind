module Solar.Caster.Utility.Time where

import Control.Concurrent.STM

data Ticker = Ticker
    { ticks :: TVar Integer
    , toTick :: Integer
    }
instance Eq Ticker where
    a == b = (toTick a == toTick b)

instance Ord Ticker where
    compare a b = compare (toTick a) (toTick b)

data TickerD = TickerD
    { ticker :: Ticker
    , remaining :: Integer
    } deriving (Eq)

instance Ord TickerD where
    compare a b = compare (remaining a) (remaining b)

