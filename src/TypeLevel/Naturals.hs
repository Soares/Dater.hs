{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module TypeLevel.Naturals where
import Control.Arrow
import Data.Ratio ((%))
import Language.Haskell.TH hiding (Pred)

data Zero = Zero
data Succ a = Z { _n :: Integer }

instance Show Zero where show = const "0"

class Natural n
    where natural :: n -> Integer
instance Natural Zero
    where natural = const 0
instance Natural n => Natural (Succ n)
    where natural = const $ 1 + natural (undefined :: n)

class Pred n m | n -> m, m -> n
instance Pred (Succ Zero) Zero
instance Pred (Succ n) m => Pred (Succ (Succ n)) (Succ m)


class Natural z => PosInt z where
    n :: z -> Integer

instance Natural z => PosInt (Succ z) where
    n = _n

instance Natural n => Enum (Succ n) where
    toEnum = Z . fromIntegral
    fromEnum = fromIntegral . n
instance Natural n => Bounded (Succ n) where
    minBound = Z 0
    maxBound = Z $ natural (undefined :: n)
instance Eq (Succ n) where
    (Z x) == (Z y) = x == y
instance Ord (Succ n) where
    (Z x) <= (Z y) = x <= y
instance Natural n => Num (Succ n) where
    (Z x) + (Z y) = Z ((x + y) `mod` m) where m = natural (undefined :: Succ n)
    (Z x) * (Z y) = Z ((x * y) `mod` m) where m = natural (undefined :: Succ n)
    (Z x) - (Z y) = Z ((x - y) `mod` m) where m = natural (undefined :: Succ n)
    fromInteger x = Z (x `mod` m) where m = natural (undefined :: Succ n)
    signum = const 1
    abs = id
instance Natural n => Real (Succ n) where
    toRational (Z x) = x % 1
instance Natural n => Integral (Succ n) where
    toInteger (Z x) = x
    quotRem (Z x) (Z y) = (fromInteger *** fromInteger) (quotRem x y)
instance Show (Succ n) where
    show (Z x) = show x

zMod :: Int -> TypeQ
zMod 0 = [t|Zero|]
zMod x = [t|Succ ($(zMod $ x-1))|]


type N0  = Zero
type N1  = Succ N0
type N2  = Succ N1
type N3  = Succ N2
type N4  = Succ N3
type N5  = Succ N4
type N6  = Succ N5
type N7  = Succ N6
type N8  = Succ N7
type N9  = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15
type N17 = Succ N16
type N18 = Succ N17
type N19 = Succ N18
type N20 = Succ N19
type N21 = Succ N20
type N22 = Succ N21
type N23 = Succ N22
type N24 = Succ N23
type N25 = Succ N24
