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
    name n = months !! fromIntegral m where m = ((n `mod` 12) - 1) `mod` 13
instance Loadable Month where
    names = zip [1..] months
months :: [String]
months =
    [ "January"
    , "February"
    , "March"
    , "April"
    , "May"
    , "June"
    , "July"
    , "August"
    , "September"
    , "October"
    , "November"
    , "December"
    ]



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
    qualify "am" 12 = 0
    qualify "pm" n | h < 12 = h + 12 where h = normal n
    qualify _ n = n
    name = show . adjust . (`mod` 12) . normal
        where adjust n = if n == 0 then 12 else n
instance Loadable Hour where qualifiers _ = ["am", "pm"]


type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60
type Gregorian = DateTime YMD HMS


instance Formatter Gregorian Standard where
    -- load :: Map (Target Standard) Integer -> Gregorian
    loader _ Standard.Year = undefined :: Year
    loader _ Standard.Month = undefined :: Month
    loader _ Standard.Day = undefined :: Day
    loader _ Standard.Hour = undefined :: Hour
    loader _ Standard.Minute = undefined :: N60
    loader _ Standard.Second = undefined :: N60
    loader _ _ = undefined :: Integer
