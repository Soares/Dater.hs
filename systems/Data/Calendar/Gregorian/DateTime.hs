{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.DateTime where
import Data.Calendar.Composers
import Data.Pair
import Data.Calendar.Gregorian.Date (Date, Year, Month, Day)
import qualified Data.Calendar.Gregorian.Date as Date
import Data.Calendar.Gregorian.Time (Time, Hour, Minute, Second)
import Prelude hiding (fst, snd)

type DateTime = Date :/ Hour :/ Minute :\ Second

date :: DateTime -> Date
date = fst.fst.fst

time :: DateTime -> Time
time (_:/h:/m:\s) = h:/m:/fromIntegral s

mkDateTime :: Date -> Time -> DateTime
mkDateTime d (h:/m:/s) = d:/h:/m:\fromIntegral s

year :: DateTime -> Year
year = Date.year . date

month :: DateTime -> Month
month = Date.month . date

day :: DateTime -> Day
day = Date.day . date

hour :: DateTime -> Hour
hour = snd.fst.fst

minute :: DateTime -> Minute
minute = snd.fst

second :: DateTime -> Second
second = snd

meridian :: DateTime -> String
meridian g = if hour g >= 12 then "pm" else "am"
