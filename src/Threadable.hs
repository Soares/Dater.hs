{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Threadable where

class Threadable a x y | a -> x, a -> y where
    thread :: x -> (a, y)
    unthread :: a -> y -> x
