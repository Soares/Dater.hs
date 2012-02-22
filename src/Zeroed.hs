{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Zeroed where

class Zeroed a where zero :: a
instance Zeroed Integer where zero = 0
instance Zeroed Rational where zero = 0
instance Zeroed Int where zero = 0
