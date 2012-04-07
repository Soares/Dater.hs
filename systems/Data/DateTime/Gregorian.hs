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
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Text.Chunk
import Test.QuickCheck
import Text.Parse
import Text.Printf (printf)


newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Parse, Normalize, Arbitrary, Modable, Displayable)
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
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Random, Modable)
instance Relable Month where type Relative Month = Maybe Month
instance Arbitrary Month where arbitrary = maxMag 1000
instance Show Month where show (M m) = printf "%02d" m
instance Ranged Month Year where range = const (1, 12)
instance Displayable Month where
    number = toInteger
    name 1 = "January"
    name 2 = "February"
    name 3 = "March"
    name 4 = "April"
    name 5 = "May"
    name 6 = "June"
    name 7 = "July"
    name 8 = "August"
    name 9 = "September"
    name 10 = "October"
    name 11 = "November"
    name 12 = "December"
    name n = if n == 0 then name (12::Month) else name (n `mod` 12)


newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Random, Modable, Displayable)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31

newtype Hour = H N24 deriving
    (Eq, Ord, Num, Real, Enum, Integral, Bounded, Parse, Normalize, Modable)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour
instance Displayable Hour where
    number = toInteger
    name n = from (normal n `mod` 12) where
        from x = show $ if x == 0 then 12 else x


type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60
type Gregorian = DateTime YMD HMS
