{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import VarPart
import Range
import Zeroed
import Prelude hiding (Enum(..))
import FullEnum
import qualified Prelude

newtype Year = Y Integer deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Show, Zeroed)
newtype Month = M Integer deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Show, Zeroed)
newtype Day = D Integer deriving (Eq, Ord, Num, Integral, Enum, Prelude.Enum, Real, Show, Zeroed)

isLeapYear :: Year -> Bool
isLeapYear y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False

instance Range Year Month where
    range = const (1, 12)
instance Range (Year:/:Month) Day where
    start = const 1
    end (y:/:2) = if isLeapYear y then 29 else 28
    end (_:/:9) = 30
    end (_:/:4) = 30
    end (_:/:6) = 30
    end (_:/:10) = 30
    end _ = 31

main = do
    print (toEnum (365*2012) :: (Year:/:Month:/:Day))
    print (toEnum (365*2013) :: (Year:/:Month:/:Day))
