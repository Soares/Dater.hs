{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.DateTime.Gregorian.Time where
import Data.DateTime
import Data.DateTime.Gregorian.Date -- TODO
import Data.Modable
import Data.Naturals
import Data.Normalize
import Test.QuickCheck
import Text.Format.Read
import Text.Format.Write
import Text.Printf (printf)

-- TODO: Leap Seconds

newtype Hour = H N24 deriving
    ( Eq, Ord, Num, Real, Enum, Integral
    , Bounded, Normalize, Modable, ReadBlock, WriteBlock, Arbitrary)
instance Show Hour where show (H h) = printf "%02d" h
instance Relable Hour where type Relative Hour = Maybe Hour

type Time = Hour ::: N60 ::: N60
