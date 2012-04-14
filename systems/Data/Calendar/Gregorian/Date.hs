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
    , mkDate
    , year
    , month
    , day
    , daysInYear
    , leapDaysNearerZero
    ) where
import Data.BoundedIn
import Data.Calendar
import Data.Calendar.Utils (maxMag)
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Pair
import Prelude hiding (fst, snd, curry)
import System.Random
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write

type Date = Year :/ Month :\ Day

mkDate :: Int -> Int -> Int -> Date
mkDate y m d = normal $ shifted y :/ shifted m :\ shifted d where

shifted :: forall x y. (Integral x, Integral y) => x -> y
shifted = fromIntegral . pred

year :: Date -> Year
year = fst.fst

month :: Date -> Month
month = snd.fst

day :: Date -> Day
day = snd

newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Normalize, Arbitrary, Modable, WriteBlock, ReadBlock)
instance Relable Year where type Relative Year = Maybe Year
instance Show Year where show (Y y) = show (y + 1) -- Year 0 = 1AD

isLeapYear :: Year -> Bool
isLeapYear y -- Remember, Y 0 == 1AD, so Y 1999 = 2000AD etc.
    | y < 7 = y `elem` negLeapYears
    | y <= 1599 = y `mod` 4 == 3
    | y `mod` 400 == 3 = True
    | y `mod` 100 == 3 = False
    | y `mod` 4 == 3 = True
    | otherwise = False

negLeapYears :: [Year]
negLeapYears = map bc [9, 12 .. 45] where bc = Y . succ . negate

posLeapYears :: [Year]
posLeapYears = filter isLeapYear [7, 11 ..]

leapDaysNearerZero :: Date -> Int
leapDaysNearerZero d = length $ takeWhile nearerZero ys where
    leapDay y = y :/ M 1 :\ D 28
    ys = map leapDay $ if d < 0 then negLeapYears else posLeapYears
    nearerZero x = if d < 0 then x > d else x < d

daysInYear :: Year -> Integer
daysInYear y = if isLeapYear y then 366 else 365


newtype Month = M N12 deriving
    (Eq, Ord, Num, Real, Enum, Integral, Arbitrary, Bounded, Normalize, Modable)
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
    numerical (M m) = show (normal m + 1)
    textual = show
monthParsers :: [ParseBlock]
monthParsers = zipWith (curry intStrParser) [1..] (map show months)
    where months = [minBound .. maxBound :: Month]

monthDay :: Date -> Int
monthDay d = case normal d of
    -- October 5-14, 1582 never happened.
    -- TODO: check that 4 Oct 1582 is a Thursday, 15 Oct 1582 a Friday
    (Y 1581 :/ M 9 :\ D d) -> if d < 4 then d + 1 else d + 11
    (_ :/ _ :\ D d) -> d + 1
-- TODO
class ShowIn a x where showIn :: x -> a -> String

newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = show (d + 1)
instance BoundedIn Day (Year:/Month) where
    start _ = 0
    end (y:/m)
        | m < minBound || m > maxBound = end $ normal (y:/m)
        | y == Y 1581 && m == M 9 = 20 -- October 1582 had 20 days
    end (y:/1) = if isLeapYear y then D 28 else D 27
    end (_:/m) = if m `elem` [8,3,5,9] then D 29 else D 30
