{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.DateTime.Gregorian.Date where
import Data.DateTime
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write
import Text.Printf (printf)

-- TODO: Month is const

type Date = Year ::: Month :/: Day


newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Normalize, Arbitrary, Modable, WriteBlock, ReadBlock)
instance Relable Year where type Relative Year = Maybe Year
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y
isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False


months :: [String]
months =
    [ "January"
    , "February"
    , "March"
    , "April"
    , "May"
    , "June"
    , "July"
    , "August", "September"
    , "October"
    , "November"
    , "December"
    ]
newtype Month = M N12 deriving
    (Eq, Ord, Num, Real, Enum, Integral, Arbitrary, Modable, Bounded)
instance Relable Month where type Relative Month = Maybe Month
instance Show Month where show (M m) = printf "%02d" m
instance WriteBlock Month where
    numerical = show . toInteger
    textual m = months !! (fromIntegral i - 1) where
        i = if m `mod` 12 == 0 then 12 else m `mod` 12
monthParsers :: [ParseBlock]
monthParsers = map intStrParser $ zip [(1::Integer)..] months


newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:::Month) where
    start = const 1
    end (y:::2) = if isLeapYear y then 29 else 28
    end (_:::m) = if m `elem` [9,4,6,10] then 30 else 31
