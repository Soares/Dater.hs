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
import Data.Calendar.Gregorian.Weeks
import qualified Data.Calendar.Gregorian.DateTime as DateTime
import Data.Calendar.Gregorian.TimeZones
import Data.Ratio (numerator, denominator)
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

instance Formatter Gregorian Standard.Standard where
    formattable Standard.DateTime _ = Write
    formattable Standard.Date _ = Write . date
    formattable Standard.Time _ = Write . time
    formattable Standard.TimeZone Nothing = nonlocalTimeZoneStyles . zone
    formattable Standard.TimeZone (Just loc) = tzs <$> zloc <*> zone where
        tzs = localTimeZoneStyles
        zloc = At (place loc) <$> dateTime
    formattable Standard.Century _ = Write . (`div` 100) . year
    formattable Standard.Year _ = Write . map toInteger . ys where
        ys d = [year d, weekYear $ date d]
    formattable Standard.Month _ = Write . month
    formattable Standard.MonthDay _ = Write . day
    formattable Standard.Week _ = Write . ws . date where
        ws d = [weekInYearMonFirst d, weekInYearSunFirst d, weekInWeekYear d]
    formattable Standard.WeekDay _ = Write . ws . date where
        ws d = map (\num->(num $ weekDay d, show $ weekDay d)) styles
        styles = [numOfWeekMonFirst, numOfWeekSunFirst]
    formattable Standard.Hour _ = Write . hour
    formattable Standard.Meridium _ = Write . meridian
    formattable Standard.Minute _ = Write . minute
    formattable Standard.Second _ = Write . second
    formattable Standard.Fraction _ = writeIntStr . (i &&& s) where
        writeIntStr = Write :: (Integer, String) -> Writers
        i = (10^(9::Int) *) . floor . extra
        s = printf "%d/%d" <$> (numerator.extra) <*> (denominator.extra)
