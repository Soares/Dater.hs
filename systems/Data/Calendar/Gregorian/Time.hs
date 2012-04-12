{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.Time where
import Control.Arrow (first)
import Data.Pair
import Data.Calendar
import Data.Calendar.Gregorian.Date
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import System.Random
import Test.QuickCheck hiding (elements) -- TODO: other way around.
import Text.Format.Read
import Text.Format.Write
import Text.Printf (printf)

import Debug.Trace (trace) -- TODO
    
type Time = Hour ::: Minute ::: N60

hour :: Time -> Hour
hour = left.left

minute :: Time -> Minute
minute = right.left

second :: Time -> N60
second = right

newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour

newtype Minute = P N60 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Minute where show (P p) = printf "%02d" p
instance Relable Minute where type Relative Minute = Maybe Minute

newtype Second = S Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Second where type Relative Second = Maybe Second
instance Arbitrary Second where arbitrary = maxMag 10
instance Show Second where show (S d) = show d
instance Ranged Second (Date:::Hour:::Minute) where
    start = const 0
    end ymdhm = fromInteger $ (leapsIn leapSeconds ymdhm) + 59
    -- TODO: Why the hell is normalize so slow?
    -- TODO: we can do way faster than this.
    -- Simply do the multiplications, then calculate the leap days & seconds
    -- TODO: find the leap second
    size dt@(y:::mo:/:d:::h:::mi) s = let
        sy = sum $ map secondsInYear predsY
        smo = sum $ map secondsInMonth [y:::x | x <- predsB mo]
        sd = sum $ map secondsInDay [y:::mo:/:x | x<-predsR (y:::mo) d]
        sh = sum $ map secondsInHour [y:::mo:/:d:::x | x <- predsB h]
        smi = sum $ map secondsInMinute [y:::mo:/:d:::h:::x|x<-predsB mi]
        ss = intify dt s
        predsY = if y < 0 then [y+1..pred 0] else [0..y-1]
        predsB :: forall a. (Integral a, Bounded a) => a -> [a]
        predsB a = if y < 0 then [a+1..maxBound] else [minBound..a-1]
        predsR :: forall a x. (Integral a, Ranged a x) => x -> a -> [a]
        predsR x a = if y < 0 then [a+1..end x] else [start x..a-1]
        in signed y ((trace (printf "%d from year" sy) sy)
            + (trace (printf "%d from month" smo) smo)
            + (trace (printf "%d from day" sd) sd)
            + (trace (printf "%d from hour" sh) sh)
            + (trace (printf "%d from minute" smi) smi)
            + (trace (printf "%d from second" ss) ss))
    split i = let
        (y, j) = search secondsInYear (elems i) (tr "i" i)
        (ym, k) = search secondsInMonth [y:::mo | mo <- [minBound..maxBound]] (tr "j" j)
        (ymd, l) = search secondsInDay [ym:/:d | d <- elements ym] (tr "k" k)
        (ymdh, m) = search secondsInHour [ymd:::h | h <- [minBound..maxBound]] (tr "l" l)
        in search secondsInMinute [ymdh:::mi | mi <- [minBound..maxBound]] (tr "m" m)

tr str v = trace (printf "%d %s" (toInteger v) str) v

leapSeconds :: [(Date:::Hour:::Minute, Integer)]
leapSeconds =
    [ (on 1972  6 30 ::: 23 ::: 59, 1)
    , (on 1972 12 31 ::: 23 ::: 59, 1)
    , (on 1973 12 31 ::: 23 ::: 59, 1)
    , (on 1974 12 31 ::: 23 ::: 59, 1)
    , (on 1976 12 31 ::: 23 ::: 59, 1)
    , (on 1976 12 31 ::: 23 ::: 59, 1)
    , (on 1977 12 31 ::: 23 ::: 59, 1)
    , (on 1978 12 31 ::: 23 ::: 59, 1)
    , (on 1979 12 31 ::: 23 ::: 59, 1)
    , (on 1981  6 30 ::: 23 ::: 59, 1)
    , (on 1982  6 30 ::: 23 ::: 59, 1)
    , (on 1983  6 30 ::: 23 ::: 59, 1)
    , (on 1986  6 30 ::: 23 ::: 59, 1)
    , (on 1987 12 31 ::: 23 ::: 59, 1)
    , (on 1989 12 31 ::: 23 ::: 59, 1)
    , (on 1990 12 31 ::: 23 ::: 59, 1)
    , (on 1992  6 30 ::: 23 ::: 59, 1)
    , (on 1993  6 30 ::: 23 ::: 59, 1)
    , (on 1994  6 30 ::: 23 ::: 59, 1)
    , (on 1996 12 31 ::: 23 ::: 59, 1)
    , (on 1997  6 30 ::: 23 ::: 59, 1)
    , (on 1998 12 31 ::: 23 ::: 59, 1)
    , (on 2006 12 31 ::: 23 ::: 59, 1)
    , (on 2008 12 31 ::: 23 ::: 59, 1)
    , (on 2012  6 30 ::: 23 ::: 59, 1)
    ] where on = mkDate :: Int -> Int -> Int -> Date

leapSecondYears :: [(Year, Integer)]
leapSecondYears = map (first $ left.left.left.left) leapSeconds
leapSecondMonths :: [(Year:::Month, Integer)]
leapSecondMonths = map (first $ left.left.left) leapSeconds
leapSecondDays :: [(Date, Integer)]
leapSecondDays = map (first $ left.left) leapSeconds
leapSecondHours :: [(Date:::Hour, Integer)]
leapSecondHours = map (first $ left) leapSeconds

leapsIn :: Eq a => [(a, Integer)] -> a -> Integer
leapsIn xs x = sum $ map snd $ filter ((== x) . fst) xs

secondsInYear :: Year -> Integer
secondsInYear y = leapsIn leapSecondYears y + (daysInYear y) * 60 * 60 * 24

secondsInMonth :: (Year:::Month) -> Integer
secondsInMonth ym = leapsIn leapSecondMonths ym + (60 * 60 * 24) * (count ym)

secondsInDay :: Date -> Integer
secondsInDay ymd = leapsIn leapSecondDays ymd + 60 * 60 * 24

secondsInHour :: (Date:::Hour) -> Integer
secondsInHour ymdh = leapsIn leapSecondHours ymdh + 60 * 60

secondsInMinute :: (Date:::Hour:::Minute) -> Integer
secondsInMinute ymdhm = leapsIn leapSeconds ymdhm + 60
