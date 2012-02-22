{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Possibilities where
import Data.List.Zipper

class Possibilities x a | a -> x where
    possibilities :: x -> Zipper a
