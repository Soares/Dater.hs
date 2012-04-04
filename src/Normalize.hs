module Normalize ( Normalize(normalize, isNormal, isOver, isUnder)) where

class Normalize a where
    normalize :: a -> a
    isNormal :: a -> Bool
    isNormal a = not (isOver a) && not (isUnder a)
    isOver :: a -> Bool
    isUnder :: a -> Bool

instance Normalize Integer where
    normalize = id
    isOver = const False
    isUnder = const False

instance Normalize Int where
    normalize = id
    isOver = const False
    isUnder = const False
