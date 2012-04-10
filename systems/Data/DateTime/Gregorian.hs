{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.DateTime.Gregorian where
import Data.DateTime
import Data.DateTime.Gregorian.Places
import Data.DateTime.Gregorian.Date
import Data.DateTime.Gregorian.Time
import Data.DateTime.Gregorian.TimeZones
import Data.Ratio (numerator, denominator)
-- TODO: import Text.Format.Read
import Text.Format.Write
import qualified Text.Format.DateTime.Standard as Standard
import Text.Printf (printf)

type Gregorian = DateTime Date Time TimeZone
data instance Locale Gregorian = Loc { place :: Place }

instance Formatter Gregorian Standard.Standard where
    formattable _ g Standard.DateTime = out g
    formattable _ g Standard.Date = out $ date g
    formattable _ g Standard.Time = out $ time g
    formattable Nothing g Standard.TimeZone = nonlocalTimeZoneStyles $ zone g
    formattable (Just loc) g Standard.TimeZone = localTimeZoneStyles z $ zone g
        where z = At (place loc) () () -- TODO: (date g) (time g)
    formattable _ g Standard.Century = out $ y `div` 100 where
        (y:/:_:/:_) = date g
    formattable _ g Standard.Year = let (y:/:_:/:_) = date g in out y
    formattable _ g Standard.Month = let (_:/:m:/:_) = date g in out m
    formattable _ g Standard.MonthDay = let (_:/:_:/:d) = date g in out d
    formattable _ g Standard.Week = undefined --TODO
    formattable _ g Standard.WeekDay = undefined --TODO
    formattable _ g Standard.Hour = let (h:::_:::_) = time g in out h
    formattable _ g Standard.Meridium = out m where
        m = if h >= 12 then "pm" else "am"
        (h:::_:::_) = time g
    formattable _ g Standard.Minute = let (_:::m:::_) = time g in out m
    formattable _ g Standard.Second = let (_:::_:::s) = time g in out s
    formattable _ g Standard.Fraction = out (i :: Integer, s :: String) where
        x = extra g
        i = floor $ x * 10 ^ (9::Int)
        s = printf "%d/%d" (numerator x) (denominator x)
