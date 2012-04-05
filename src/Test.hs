{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
import VarPart
import ConstPart
import Date
import Gen
import Ranged
import Parse
import Zeroed
import Normalize
import Naturals
import Coded
import Text.Printf (printf)

isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False

newtype Year = Y Integer deriving (Eq, Ord, Num, Real, Enum, Integral, Parse, Gen, Normalize)
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y

newtype Month = M Int deriving (Eq, Ord, Num, Real, Enum, Integral, Parse)
instance Show Month where show (M m) = printf "%02d" m
instance Ranged Month Year where
    start = const 1
    end = const 12

newtype Day = D Int deriving (Eq, Ord, Num, Real, Enum, Integral, Parse)
instance Show Day where show (D d) = printf "%02d" d
instance Ranged Day (Year:/:Month) where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31

type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60
type Gregorian = Date YMD HMS

main :: IO ()
main = do
    print $ normal ((decode $ encode (decode (365*2012)::YMD))::YMD)
    print $ normal ((decode $ encode (decode (365*2013)::YMD))::YMD)
    print $ normal ((decode $ encode (decode (23*60*60)::HMS))::HMS)
    print $ normal ((decode $ encode (decode (12*60*60+(17*60)+59)::HMS))::HMS)
