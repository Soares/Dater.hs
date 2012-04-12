{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.Date where
import Data.Calendar
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Pair
import Data.BoundedIn
import System.Random
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write

type Date = Year :/ Month :\ Day

mkDate :: (Integral a, Integral b, Integral c) => a -> b -> c -> Date
mkDate y m d = normal $ (shifted y) :/ (shifted m) :\ (shifted d) where
    shifted :: forall x y. (Integral x, Integral y) => x -> y
    shifted = fromIntegral . pred

year :: Date -> Year
year = left.left

month :: Date -> Month
month = right.left

day :: Date -> Day
day = right

newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Normalize, Arbitrary, Modable, WriteBlock, ReadBlock)
instance Relable Year where type Relative Year = Maybe Year
instance Show Year where show (Y y) = show (y + 1) -- Year 0 = 1AD

isLeapYear :: Year -> Bool
isLeapYear y -- Remember, Y 0 == 1AD, so Y 1999 = 2000AD etc.
    | y `mod` 400 == 3 = True
    | y `mod` 100 == 3 = False
    | y `mod` 4 == 3 = True
    | otherwise = False

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
    show (M m) = show (normal m)
instance WriteBlock Month where
    numerical (M m) = show ((normal m) + 1)
    textual = show
monthParsers :: [ParseBlock]
monthParsers = map intStrParser . zip [1..] $ map show months
    where months = [minBound .. maxBound :: Month]


newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = show (d + 1)
instance BoundedIn Day (Year:/Month) where
    start = const 0
    end ym@(_:/m) | m < minBound || m > maxBound = end $ normal ym
    end (y:/2) = if isLeapYear y then D 28 else D 27
    end (_:/m) = if m `elem` [9,4,6,10] then D 29 else D 30
