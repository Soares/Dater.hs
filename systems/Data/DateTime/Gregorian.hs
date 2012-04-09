{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DateTime.Gregorian where
import Data.DateTime
import Data.Ratio (numerator, denominator)
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Test.QuickCheck
import Text.Format.DateTime.Standard (Standard)
import qualified Text.Format.DateTime.Standard as Standard
import Text.Format
import Text.Printf (printf)


newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Normalize, Arbitrary, Modable, Formattable)
instance Relable Year where type Relative Year = Maybe Year
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y
isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False


newtype Month = M Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable)
instance Relable Month where type Relative Month = Maybe Month
instance Arbitrary Month where arbitrary = maxMag 1000
instance Show Month where show (M m) = printf "%02d" m
instance Ranged Month Year where range = const (1, 12)
instance Formattable Month where
    number = toInteger
    name n = months !! (fromIntegral m - 1) where
        m = if n `mod` 12 == 0 then 12 else n `mod` 12
instance Loadable Month where
    names = const $ zip [1..] months
months :: [String]
months = ["January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December" ]



newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, Formattable)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31

newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, Formattable)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour
{-
instance Formattable Hour where
    number = toInteger
    qualify "am" 12 = 0
    qualify "pm" n | h < 12 = h + 12 where h = normal n
    qualify _ n = n
    name = show . adjust . (`mod` 12) . normal
        where adjust n = if n == 0 then 12 else n
instance Loadable Hour where qualifiers _ = ["am", "pm"]
-}


type YMD = Year :/: Month :/: Day
type HMS = Hour ::: N60   ::: N60
type Gregorian = DateTime YMD HMS

instance Formatter Gregorian Standard where
    formattable g Standard.DateTime _ = undefined -- TODO
    formattable g Standard.Date _ = undefined -- TODO
    formattable g Standard.Time _ = undefined -- TODO
    formattable g Standard.TimeZone _ = undefined -- TODO
    formattable g Standard.Century _ = undefined -- TODO
    formattable g Standard.Year _ = let (y:/:_:/:_) = day g in Out y
    formattable g Standard.Month _ = let (_:/:m:/:_) = day g in Out m
    formattable g Standard.MonthDay _ = let (_:/:_:/:d) = day g in Out d
    formattable g Standard.Week _ = undefined --TODO
    formattable g Standard.WeekDay _ = undefined --TODO
    formattable g Standard.Hour _ = let (h:::_:::_) = time g in Out h
    formattable g Standard.Meridium _ = undefined -- TODO
    formattable g Standard.Minute _ = let (_:::m:::_) = time g in Out m
    formattable g Standard.Second _ = let (_:::_:::s) = time g in Out s
    formattable g Standard.Fraction _ = Out (i :: Integer, s :: String) where
        x = extra g
        i = floor $ x * 10 ^ (9::Int)
        s = printf "%d/%d" (numerator x) (denominator x)
