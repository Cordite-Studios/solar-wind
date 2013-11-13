{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solar.Utility.Date where

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar
import Data.Time.Format()
import Data.Monoid

instance Monoid UTCTime where
    mempty = UTCTime (fromGregorian 0 0 0) 0
    mappend u u'
        | u > u' = u
        | u < u' = u'
        | otherwise = u'
