{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.DateTime.Gregorian.Time where
import Data.DateTime
import Data.DateTime.Gregorian.Date
import Data.Modable
import Data.Naturals
import Data.Normalize
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write
import Text.Printf (printf)

type DateTime = Date ::: Hour ::: Minute :/: Second

newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour

type Minute = N60

newtype Second = S Int deriving
    (Eq, Ord, Num, Real, Enum, Integral, Random, Modable, ReadBlock, WriteBlock)
instance Relable Second where type Relative Second = Maybe Second
instance Arbitrary Second where arbitrary = maxMag 1000
instance Show Second where show (S d) = printf "%02d" d
instance Ranged Second (Date:::Hour:::Minute) where
    start = const 0
    end (d:::h:::m) = 59 + leapSecond d h m
leapSecond :: Date -> Hour -> Minute -> Second
leapSecond _ _ _ = 0
