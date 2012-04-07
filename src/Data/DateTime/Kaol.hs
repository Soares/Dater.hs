{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DateTime.Kaol where
import Data.DateTime.ConstPart
import Data.DateTime.DateTime
import Data.DateTime.VarPart
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Test.QuickCheck
import Text.Parse
import Text.Printf (printf)

newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Parse, Normalize, Arbitrary, Modable)
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
instance Ranged Month Year where range = const (0, 12)


newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Random, Modable)
instance Relable Day where type Relative Day = Maybe Day
instance Arbitrary Day where arbitrary = maxMag 1000
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start (_:/:m) = if m == 0 then 0 else 1
    end (y:/:0) = if isLeapYear y then 1 else 0
    end (_:/:m) = if m `elem` [1,4,7,10] then 31 else 30


type YMD = Year :/: Month :/: Day
type HMS = N10  ::: N100 ::: N100
type Kaol = DateTime YMD HMS
