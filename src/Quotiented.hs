{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quotiented
    ( varQuotRem
    ) where
import Control.Applicative
import Gen
import Range

varQuotRem :: (a -> Integer) -> a -> [a] -> Integer -> (a, Integer)
varQuotRem s z as n = choose $ zip3 as (0:sizes) sizes where
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else c+n
    choose ((a, b, c):ds) = if enough c then (a, leftover b c) else choose ds
    sizes = cascade $ map s as

cascade :: Num a => [a] -> [a]
cascade (x:xs) = x : map (+x) (cascade xs)
cascade [] = []
