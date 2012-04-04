module Normalize ( Normalize(normalize, isNormal)) where

class Normalize a where
    normalize :: a -> a
    isNormal :: a -> Bool

instance Normalize Integer where
    normalize = id
    isNormal = const True

instance Normalize Int where
    normalize = id
    isNormal = const True
