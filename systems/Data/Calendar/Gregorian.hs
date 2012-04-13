{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Justified because it's a multi-param typeclass.
-- Probably. Read more on how orphans work on such things.
-- (Make sure we can make other instances of Formatter Gregorian)
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Calendar.Gregorian where
import Control.Applicative
import Control.Arrow ((&&&))
import Data.Calendar
import Data.Calendar.Gregorian.Places
import Data.Calendar.Gregorian.DateTime (DateTime)
import Data.Calendar.Gregorian.Date (Date, Year, Month, Day)
import Data.Calendar.Gregorian.Time (Time, Hour, Minute, Second)
import qualified Data.Calendar.Gregorian.DateTime as DateTime
import Data.Calendar.Gregorian.TimeZones
import Data.Ratio (numerator, denominator)
import Text.Format.Read
import Text.Format.Write
import qualified Text.Format.Calendar.Standard as Standard
import Text.Printf (printf)

type Gregorian = Calendar DateTime TimeZone
data instance Locale Gregorian = Loc { place :: Place }

date :: Gregorian -> Date
date = DateTime.date . dateTime

time :: Gregorian -> Time
time = DateTime.time . dateTime

year :: Gregorian -> Year
year = DateTime.year . dateTime

month :: Gregorian -> Month
month = DateTime.month . dateTime

day :: Gregorian -> Day
day = DateTime.day . dateTime

hour :: Gregorian -> Hour
hour = DateTime.hour . dateTime

minute :: Gregorian -> Minute
minute = DateTime.minute . dateTime

second :: Gregorian -> Second
second = DateTime.second . dateTime

meridian :: Gregorian -> String
meridian g = if hour g >= 12 then "pm" else "am"

-- TODO: Weeks
--
instance Formatter Gregorian Standard.Standard where
    formattable Standard.DateTime _ = out
    formattable Standard.Date _ = out . date
    formattable Standard.Time _ = out . time
    formattable Standard.TimeZone Nothing = nonlocalTimeZoneStyles . zone
    formattable Standard.TimeZone (Just loc) = tzs <$> zloc <*> zone where
        tzs = localTimeZoneStyles
        zloc = At (place loc) <$> dateTime
    formattable Standard.Century _ = out . (`div` 100) . year
    formattable Standard.Year _ = out . year
    formattable Standard.Month _ = out . month
    formattable Standard.MonthDay _ = out . day
    formattable Standard.Week _ = undefined
    formattable Standard.WeekDay _ = undefined
    formattable Standard.Hour _ = out . hour
    formattable Standard.Meridium _ = out . meridian
    formattable Standard.Minute _ = out . minute
    formattable Standard.Second _ = out . second
    formattable Standard.Fraction _ = writeIntStr . (i &&& s) where
        writeIntStr = out :: (Integer, String) -> Writers
        i = (10^(9::Int) *) . floor . extra
        s = printf "%d/%d" <$> (numerator.extra) <*> (denominator.extra)

instance Loader Gregorian Standard.Standard where
    loadable Standard.DateTime _ = load (undefined::Integer)
    loadable Standard.Date _ = load (undefined::Integer)
    loadable Standard.Time _ = load (undefined::Integer)
    loadable Standard.TimeZone Nothing = undefined -- TODO
    loadable Standard.TimeZone (Just _) = undefined -- TODO
    loadable Standard.Century _ = load (undefined::Integer)
    loadable Standard.Year _ = load (undefined::Integer)
    loadable Standard.Month _ = undefined -- TODO
    loadable Standard.MonthDay _ = load (undefined::Integer)
    loadable Standard.Week _ = undefined
    loadable Standard.WeekDay _ = undefined
    loadable Standard.Hour _ = load (undefined::Integer)
    loadable Standard.Meridium _ = stringsParser ["am", "pm"]
    loadable Standard.Minute _ = load (undefined::Integer)
    loadable Standard.Second _ = load (undefined::Integer)
    loadable Standard.Fraction _ = load (undefined::Integer)

    create = undefined -- TODO
