{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quotiented
    ( VQR(..)
    , VQRIn(..)
    , varQuotRem
    ) where
import Control.Applicative
import Zeroed
import FullEnum
import Sized
import Range

quotients :: Zeroed a => Integer -> [a]
quotients i = if i >= 0 then positives else negatives

positives :: Zeroed a => [a]
positives = ps zero where ps a = a : ps (next a)

negatives :: Zeroed a => [a]
negatives = ns (prev zero) where ns a = a : ns (prev a)

class (Sized a, Zeroed a) => VQR a where
    vqr :: Integer -> (a, Integer)
    vqr = varQuotRem size zero =<< quotients

class (SizedIn a x, Ranged a x) => VQRIn a x where
    vqrIn :: x -> Integer -> (a, Integer)
    -- TODO: can we use all elements here?
    -- What if we allow negative months?
    -- What does that even mean?
    vqrIn a = varQuotRem (sizeIn a) (start a) (elements a)

varQuotRem :: (a -> Integer) -> a -> [a] -> Integer -> (a, Integer)
varQuotRem s z as n = choose $ zip3 as (0:sizes) sizes where
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else c+n
    choose ((a, b, c):ds) = if enough c then (a, leftover b c) else choose ds
    sizes = cascade $ map s as

cascade :: Num a => [a] -> [a]
cascade (x:xs) = x : map (+x) (cascade xs)
cascade [] = []
