{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.DateTime where
import Data.Calendar.Composers
import Data.Pair
import Data.Calendar.Gregorian.Date (Date, Year, Month, Day)
import qualified Data.Calendar.Gregorian.Date as Date
import Data.Calendar.Gregorian.Time (Time, Hour, Minute, Second)

type DateTime = Date :/ Hour :/ Minute :\ Second

date :: DateTime -> Date
date = left.left.left

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
hour = right.left.left

minute :: DateTime -> Minute
minute = right.left

second :: DateTime -> Second
second = right

meridian :: DateTime -> String
meridian g = if hour g >= 12 then "pm" else "am"
