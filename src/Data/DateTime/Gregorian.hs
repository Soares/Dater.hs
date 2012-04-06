{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.DateTime.Gregorian where
import Data.DateTime.ConstPart
import Data.DateTime.DateTime
import Data.DateTime.VarPart
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Test.QuickCheck
import Text.Parse
import Text.Printf (printf)


-- TODO: hlint

newtype Year = Y Integer deriving
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Normalize, Arbitrary)
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y
isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False


newtype Month = M Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Random)
instance Arbitrary Month where
    arbitrary = sized $ \s -> choose (M $ - (max s 1000), M (max s 1000))
    shrink (M m) = map M (shrink m)
instance Show Month where show (M m) = printf "%02d" m
instance Ranged Month Year where
    start = const 1
    end = const 12


newtype Day = D Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Parse, Random)
instance Arbitrary Day where
    arbitrary = sized $ \s -> choose (D $ - (max s 1000), D (max s 1000))
    shrink (D d) = map D (shrink d)
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31


type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60
type Gregorian = DateTime YMD HMS
