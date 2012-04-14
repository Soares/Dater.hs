{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.Date
    ( Date
    , Year(..)
    , Month(..)
    , Day(..)
    , date
    , ymd
    , (/-)
    , (-/-)
    , year
    , month
    , day
    , daysInYear
    , leapDaysNearerZero
    ) where
import Control.Applicative
import Data.BoundedIn
import Data.Calendar
import Data.Calendar.Utils (search, signed, outTo, (.:))
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Pair
import Prelude hiding (fst, snd, curry)
import System.Random
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write

-- Note: This module follows the Gregorian calendar, which was first adopted
-- in 1582AD. Using it for dates before that point will extrapolate backwards
-- using the Gregorian calendar, which was never used. For example, the
-- Gregorian calendar does not have a February 29th in the year 1500, but the
-- Julian calendar in common use at the time did have a February 29th.
-- Obviously, the calendars will disagree on many points.

-- Don't base any implementation off of the composers here, please.
-- Not because I plan on changing the Gregorian calendar, but you know, just
-- in case. (For all I know, the Pope might declare that we aren't doing August
-- next year, and I'd need to change Month's composition to Variable...)
type Date = Year :/ Month :\ Day

date :: Year -> Month -> Day -> Date
date = (&) .: (&)

-- Break a date into tuple form
ymd :: Date -> (Year, Month, Day)
ymd = (,,) <$> year <*> month <*> day

-- Constructs a pair from 1-indexed numbers, converting them to
-- zero-indexed numbers.
-- Common use case: 1972 -/- 1 for January 1972
-- (equivalent to "Y 1971 :/ M 0")
(-/-) :: forall a b c d (&).
    (Pair (&), Integral a, Integral b, Integral c, Integral d)
    => a -> b -> (c & d)
x -/- y = shifted x & shifted y

-- Constructs a pair from a 0-indexed number (on the left) and a
-- one-indexed number (on the right).
-- Common use case: 1972 -/- 2 /- 28 for 28 Feb 1972
-- (equivalent to "Y 1971 :/ M 1 :/ D 27")
(/-) :: forall a b (&). (Pair (&), Integral b) => a -> Int -> (a & b)
x /- y = x & shifted y

year :: Date -> Year
year = fst.fst

month :: Date -> Month
month = snd.fst

day :: Date -> Day
day = snd


-- A Gregorian calendar year. Remember that Year 0 = 1 AD
newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Normalize, Arbitrary, Modable, WriteBlock, ReadBlock)

instance Relable Year where type Relative Year = Maybe Year

instance Show Year where show (Y y) = show (y + 1)

isLeapYear :: Year -> Bool
isLeapYear y -- Remember, Y 0 == 1AD, so Y 1999 = 2000AD etc.
    | y `mod` 400 == 3 = True
    | y `mod` 100 == 3 = False
    | y `mod` 4 == 3 = True
    | otherwise = False


-- The Gregorian calendar month.
newtype Month = M N12 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Arbitrary, Bounded, Normalize, Modable)

instance Relable Month where type Relative Month = Maybe Month

instance Show Month where
    show (M  0) = "January"
    show (M  1) = "February"
    show (M  2) = "March"
    show (M  3) = "April"
    show (M  4) = "May"
    show (M  5) = "June"
    show (M  6) = "July"
    show (M  7) = "August"
    show (M  8) = "September"
    show (M  9) = "October"
    show (M 10) = "November"
    show (M 11) = "December"
    show (M m) = show (m `mod` 12)

instance WriteBlock Month where
    numerical = pure . show . (+1) . toInteger . normal
    textual = (:) <$> show <*> monthAbbrs

monthAbbrs :: Month -> [String]
monthAbbrs (M 8) = ["Sept"]
monthAbbrs _ = []


-- The Gregorian calendar month day. Has nothing to do with week days.
newtype Day = D Int deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Random, Modable, ReadBlock, WriteBlock)

instance Relable Day where type Relative Day = Maybe Day
instance Show Day where show (D d) = show (d + 1)
instance Arbitrary Day where arbitrary = outTo 10000

instance BoundedIn Day (Year:/Month) where
    start = const 0
    end (y:/1) = if isLeapYear y then D 28 else D 27
    end (_:/m) = if m `elem` [8,3,5,9] then D 29 else D 30
    size ym@(y:/m) d = let
        ld = signed y $ toInteger $ leapDaysNearerZero (ym&d)
        fy = toInteger $ 365 * y
        fm = sum $ map (countIn . (Y 0:/)) [minBound..pred m]
        fd = toInteger d + ld
        in fy + fm + fd
    split i = let
        (y, j) = search daysInYear (if i >= 0 then [0..] else [-1,-2..]) i
        in search countIn (map (y:/) [minBound..maxBound]) j

leapDaysNearerZero :: Date -> Int
leapDaysNearerZero d = length $ takeWhile nearerZero $ map leapDay ys where
    leapDay y = y /- (2::Int) /- (29::Int)
    ys = filter isLeapYear $ if d < 0 then [-1, -5 ..] else [3, 7..]
    nearerZero x = if d < 0 then x > d else x < d

daysInYear :: Year -> Integer
daysInYear y = if isLeapYear y then 366 else 365


-- Helper function to convert from 1-indexed to 0-indexed
shifted :: forall x y. (Integral x, Integral y) => x -> y
shifted = fromIntegral . pred
