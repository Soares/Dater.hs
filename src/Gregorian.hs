{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gregorian where
import ConstPart
import Date
import Naturals
import Normalize
import Parse
import Ranged
import Text.Printf (printf)
import VarPart
import Zeroed

newtype Year = Y Integer deriving
    ( Eq, Ord, Num, Real, Enum, Integral, Parse, Normalize)
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y
isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False


newtype Month = M Int deriving
    ( Eq, Ord, Num, Real, Enum, Integral, Parse)
instance Show Month where show (M m) = printf "%02d" m
instance Ranged Month Year where
    start = const 1
    end = const 12


newtype Day = D Int deriving
    ( Eq, Ord, Num, Real, Enum, Integral, Parse)
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31


type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60
type Gregorian = Date YMD HMS
