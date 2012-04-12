{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.Time
    ( Time
    , Hour(..)
    , Minute(..)
    , Second(..)
    , hour
    , minute
    , second
    ) where
import Control.Arrow (first)
import Data.Pair
import Data.Calendar
import Data.Calendar.Utils (maxMag, search, signed)
import Data.Calendar.Gregorian.Date
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.BoundedIn
import System.Random
import Test.QuickCheck (Arbitrary(..))
import Text.Format.Read
import Text.Format.Write

type Time = Hour :/ Minute :/ N60

hour :: Time -> Hour
hour = left.left

minute :: Time -> Minute
minute = right.left

second :: Time -> N60
second = right

newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Hour where show (H h) = show h
instance Relable Hour where type Relative Hour = Maybe Hour

newtype Minute = P N60 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Minute where show (P p) = show p
instance Relable Minute where type Relative Minute = Maybe Minute

newtype Second = S Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Second where type Relative Second = Maybe Second
instance Arbitrary Second where arbitrary = maxMag 1000
instance Show Second where show (S d) = show d
instance BoundedIn Second (Date:/Hour:/Minute) where
    start = const 0
    end ymdhm = fromInteger $ leapsIn leapSeconds ymdhm + 59
    size ymdhm@(ymd@(y:/m:\d):/h:/p) s = let
        ld = signed y $ toInteger $ leapDaysNearerZero ymd
        ls = signed y $ toInteger $ leapSecondsNearerZero ymdhm
        fy = 365 * 86400 * (toInteger y)
        -- We count month days in Y 0 = 1AD because 1AD has no leap years,
        -- and leap years are already accounted for in `ld`
        fm = 86400 * (sum $ map (countIn . (Y 0:/)) [minBound..pred m])
        fd = 86400 * ((toInteger d) + ld)
        fh = 3600 * (toInteger h)
        fp = 60 * (toInteger p)
        fs = (toInteger s) + ls
        in fy + fm + fd + fh + fp + fs
    split i = let
        (y, j) = search secondsInYear (if i >= 0 then [0..] else [-1,-2..]) i
        (ym, k) = search secondsInMonth (map (y:/) [minBound..maxBound]) j
        (ymd, l) = search secondsInDay (map (ym:\) [start ym..end ym]) k
        (ymdh, m) = search secondsInHour (map (ymd:/) [minBound..maxBound]) l
        in search secondsInMinute (map (ymdh:/) [minBound..maxBound]) m

leapSecondYears :: [(Year, Integer)]
leapSecondYears = map (first $ left.left.left.left) leapSeconds
leapSecondMonths :: [(Year:/Month, Integer)]
leapSecondMonths = map (first $ left.left.left) leapSeconds
leapSecondDays :: [(Date, Integer)]
leapSecondDays = map (first $ left.left) leapSeconds
leapSecondHours :: [(Date:/Hour, Integer)]
leapSecondHours = map (first left) leapSeconds

leapSecondsNearerZero :: (Date:/Hour:/Minute) -> Int
leapSecondsNearerZero dhm
    | dhm > 0 = length $ takeWhile ((< dhm) . fst) leapSeconds
    | otherwise = 0

leapsIn :: Eq a => [(a, Integer)] -> a -> Integer
leapsIn xs x = sum $ map snd $ filter ((== x) . fst) xs

secondsInYear :: Year -> Integer
secondsInYear y = leapsIn leapSecondYears y + daysInYear y * 60 * 60 * 24

secondsInMonth :: (Year:/Month) -> Integer
secondsInMonth ym = leapsIn leapSecondMonths ym + (60 * 60 * 24) * countIn ym

secondsInDay :: Date -> Integer
secondsInDay ymd = leapsIn leapSecondDays ymd + 60 * 60 * 24

secondsInHour :: (Date:/Hour) -> Integer
secondsInHour ymdh = leapsIn leapSecondHours ymdh + 60 * 60

secondsInMinute :: (Date:/Hour:/Minute) -> Integer
secondsInMinute ymdhm = leapsIn leapSeconds ymdhm + 60

leapSeconds :: [(Date:/Hour:/Minute, Integer)]
leapSeconds =
    [ (on 1972  6 30 :/ 23 :/ 59, 1)
    , (on 1972 12 31 :/ 23 :/ 59, 1)
    , (on 1973 12 31 :/ 23 :/ 59, 1)
    , (on 1974 12 31 :/ 23 :/ 59, 1)
    , (on 1976 12 31 :/ 23 :/ 59, 1)
    , (on 1976 12 31 :/ 23 :/ 59, 1)
    , (on 1977 12 31 :/ 23 :/ 59, 1)
    , (on 1978 12 31 :/ 23 :/ 59, 1)
    , (on 1979 12 31 :/ 23 :/ 59, 1)
    , (on 1981  6 30 :/ 23 :/ 59, 1)
    , (on 1982  6 30 :/ 23 :/ 59, 1)
    , (on 1983  6 30 :/ 23 :/ 59, 1)
    , (on 1986  6 30 :/ 23 :/ 59, 1)
    , (on 1987 12 31 :/ 23 :/ 59, 1)
    , (on 1989 12 31 :/ 23 :/ 59, 1)
    , (on 1990 12 31 :/ 23 :/ 59, 1)
    , (on 1992  6 30 :/ 23 :/ 59, 1)
    , (on 1993  6 30 :/ 23 :/ 59, 1)
    , (on 1994  6 30 :/ 23 :/ 59, 1)
    , (on 1996 12 31 :/ 23 :/ 59, 1)
    , (on 1997  6 30 :/ 23 :/ 59, 1)
    , (on 1998 12 31 :/ 23 :/ 59, 1)
    , (on 2006 12 31 :/ 23 :/ 59, 1)
    , (on 2008 12 31 :/ 23 :/ 59, 1)
    , (on 2012  6 30 :/ 23 :/ 59, 1)
    ] where on = mkDate :: Int -> Int -> Int -> Date
