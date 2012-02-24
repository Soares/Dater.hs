{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import VarPart
import ConstPart
import Date
import Range
import Parse
import Zeroed
import Prelude hiding (Enum(..))
import FullEnum
import TypeLevel.Naturals
import qualified Prelude
import Text.Printf (printf)

newtype Year = Y Integer deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Parse)
instance Zeroed Year where zero = Y 1
instance Show Year where show (Y y) = show y

newtype Month = M Int deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Parse)
instance Show Month where show (M m) = printf "%02d" m

newtype Day = D Int deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Parse)
instance Show Day where show (D d) = printf "%02d" d

type YMD = Year :/: Month :/: Day
type HMS = N24  ::: N60   ::: N60

type Gregorian = Date YMD HMS

isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False

instance Range Year Month where
    start = const 1
    end = const 12
instance Range (Year:/:Month) Day where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:m) = if m `elem` [9,4,6,10] then 30 else 31

main :: IO ()
main = do
    print (toEnum (365*2012) :: (Year:/:Month:/:Day))
    print (toEnum (365*2013) :: (Year:/:Month:/:Day))
