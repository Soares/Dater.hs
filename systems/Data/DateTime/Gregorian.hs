{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Justified because it's a multi-param typeclass.
-- Probably. Read more on how orphans work on such things.
-- (Make sure we can make other instances of Formatter Gregorian)
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.DateTime.Gregorian where
import Control.Applicative
import Control.Arrow ((&&&))
import Data.DateTime
import Data.DateTime.Gregorian.Places
import Data.DateTime.Gregorian.Date
import Data.DateTime.Gregorian.Time
import Data.DateTime.Gregorian.TimeZones
import Data.Pair
import Data.Naturals
import Data.Ratio (numerator, denominator)
-- TODO: import Text.Format.Read
import Text.Format.Write
import qualified Text.Format.DateTime.Standard as Standard
import Text.Printf (printf)

type Gregorian = DateTime Date Time TimeZone
data instance Locale Gregorian = Loc { place :: Place }

year :: Gregorian -> Year
year = left . left . date

month :: Gregorian -> Month
month = right . left . date

day :: Gregorian -> Day
day = right . date

hour :: Gregorian -> Hour
hour = left . left . time

minute :: Gregorian -> N60
minute = right . left . time

second :: Gregorian -> N60
second = right . time

meridian :: Gregorian -> String
meridian g = if hour g >= 12 then "pm" else "am"

instance Formatter Gregorian Standard.Standard where
    formattable Standard.DateTime _ = out
    formattable Standard.Date _ = out . date
    formattable Standard.Time _ = out . time
    formattable Standard.TimeZone Nothing = nonlocalTimeZoneStyles . zone
    formattable Standard.TimeZone (Just loc) = tzs <$> zloc <*> zone where
        tzs = localTimeZoneStyles
        zloc = At (place loc) <$> date <*> time
    formattable Standard.Century _ = out . (`div` 100) . year
    formattable Standard.Year _ = out . year
    formattable Standard.Month _ = out . month
    formattable Standard.MonthDay _ = out . day
    formattable Standard.Week _ = undefined --TODO
    formattable Standard.WeekDay _ = undefined --TODO
    formattable Standard.Hour _ = out . hour
    formattable Standard.Meridium _ = out . meridian
    formattable Standard.Minute _ = out . minute
    formattable Standard.Second _ = out . second
    formattable Standard.Fraction _ = writeIntStr . (i &&& s) where
        writeIntStr = out :: (Integer, String) -> Writers
        i = (10^(9::Int) *) . floor . extra
        s = printf "%d/%d" <$> (numerator.extra) <*> (denominator.extra)
