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
data Succ a = Z { n :: Integer }

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

instance Natural n => Enum (Succ n) where
    toEnum = Z . fromIntegral
    fromEnum = fromIntegral . n
instance Natural n => Bounded (Succ n) where
    minBound = Z 0
    maxBound = Z $ 1 + natural (undefined :: n)
instance Natural n => Eq (Succ n) where
    (Z x) == (Z y) = x == y
instance Natural n => Ord (Succ n) where
    (Z x) <= (Z y) = x <= y
instance Natural n => Num (Succ n) where
    (Z x) + (Z y) = Z ((x + y) `mod` m) where Z m = (maxBound :: Succ n)
    (Z x) * (Z y) = Z ((x * y) `mod` m) where Z m = (maxBound :: Succ n)
    (Z x) - (Z y) = Z ((x - y) `mod` m) where Z m = (maxBound :: Succ n)
    fromInteger x = Z (x `mod` m) where Z m = (maxBound :: Succ n)
    signum = const 1
    abs = id
instance Natural n => Real (Succ n) where
    toRational (Z x) = x % 1
instance Natural n => Integral (Succ n) where
    toInteger (Z x) = x
    quotRem (Z x) (Z y) = (fromInteger *** fromInteger) (quotRem x y)
instance Natural n => Show (Succ n) where
    show (Z x) = show x


zMod :: Int -> TypeQ
zMod 0 = [t|Zero|]
zMod n = [t|Succ ($(zMod $ n-1))|]
