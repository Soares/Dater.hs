-- TODO: Cleanup
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DateTime.Gregorian.Time where
import Data.DateTime
import Data.DateTime.Gregorian.Places
import Data.DateTime.Gregorian.TimeZones
import Data.Ratio (numerator, denominator)
import Data.Modable
import Data.Naturals
import Data.Normalize
import Data.Ranged
import Data.Zeroed
import System.Random
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write
import Text.Format.DateTime.Standard (Standard)
import qualified Text.Format.DateTime.Standard as Standard
import Text.Printf (printf)


newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour

{- TODO: Read meridian
instance Formattable Hour where
    number = toInteger
    qualify "am" 12 = 0
    qualify "pm" n | h < 12 = h + 12 where h = normal n
    qualify _ n = n
    name = show . adjust . (`mod` 12) . normal
        where adjust n = if n == 0 then 12 else n
instance Loadable Hour where qualifiers _ = ["am", "pm"]
-}

type Time = Hour ::: N60   ::: N60
