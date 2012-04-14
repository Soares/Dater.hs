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
    , time
    , hms
    , hour
    , minute
    , second
    , leapSeconds
    ) where
import Control.Applicative
import Data.Pair hiding (second)
import Data.Calendar
import Data.Calendar.Utils (outTo, search, signed, (.:))
import Data.Calendar.Gregorian.Date
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.BoundedIn
import Prelude hiding (fst, snd)
import System.Random
import Test.QuickCheck (Arbitrary(..))
import Text.Format.Read
import Text.Format.Write

type Time = Hour :/ Minute :/ N60

time :: Hour -> Minute -> N60 -> Time
time = (&) .: (&)

hms :: Time -> (Hour, Minute, N60)
hms = (,,) <$> hour <*> minute <*> second

hour :: Time -> Hour
hour = fst.fst

minute :: Time -> Minute
minute = snd.fst

second :: Time -> N60
second = snd


-- Gregorian calendar Hour
newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)

instance Show Hour where show (H h) = show h

instance Relable Hour where type Relative Hour = Maybe Hour


-- Gregorian calendar minute
newtype Minute = P N60 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)

instance Show Minute where show (P p) = show p

instance Relable Minute where type Relative Minute = Maybe Minute


-- Gregorian calendar second, respects leap seconds.
-- If you want a Date-independant Second, use N60.
newtype Second = S Int deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Random, Modable, ReadBlock, WriteBlock)

instance Relable Second where type Relative Second = Maybe Second

instance Arbitrary Second where arbitrary = outTo 3600

instance Show Second where show (S d) = show d

instance BoundedIn Second (Date:/Hour:/Minute) where
    start = const 0
    end ymdhm = fromInteger $ leapsIn leapSeconds ymdhm + 59
    -- This code is ugly, but necessary for performance reasons.
    -- Unless you want to count the number of seconds since 1AD by enumerating
    -- the minutes, I suppose... (which is the only way to do it W.L.G, i.e.
    -- in the default definition)
    size dthm@(dt@(y:/m:\d):/h:/p) s = let
        ld = signed y $ toInteger $ leapDaysNearerZero dt
        ls = signed y $ toInteger $ leapSecondsNearerZero dthm
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
        (d, l) = search secondsInDay (map (ym:\) [start ym..end ym]) k
        (dh, m) = search secondsInHour (map (d:/) [minBound..maxBound]) l
        in search secondsInMinute (map (dh:/) [minBound..maxBound]) m

-- Leap second utilities
leapSecondYears :: [(Year, Integer)]
leapSecondYears = map (first $ fst.fst.fst.fst) leapSeconds
leapSecondMonths :: [(Year:/Month, Integer)]
leapSecondMonths = map (first $ fst.fst.fst) leapSeconds
leapSecondDays :: [(Date, Integer)]
leapSecondDays = map (first $ fst.fst) leapSeconds
leapSecondHours :: [(Date:/Hour, Integer)]
leapSecondHours = map (first fst) leapSeconds

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
secondsInDay d = leapsIn leapSecondDays d + 60 * 60 * 24

secondsInHour :: (Date:/Hour) -> Integer
secondsInHour dh = leapsIn leapSecondHours dh + 60 * 60

secondsInMinute :: (Date:/Hour:/Minute) -> Integer
secondsInMinute ymdhm = leapsIn leapSeconds ymdhm + 60

-- A table of all declared leap seconds.
-- Must be kept in synch with the planet.
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
    ] where on = (/-) .: (-/-) :: Int -> Int -> Int -> Date
