module Data.Coded (Coded(..)) where

-- | Things that can be encoded and decoded
class Coded a where
    encode :: a -> Integer
    decode :: Integer -> a

instance Coded Integer where
    encode = id
    decode = id

instance Coded Int where
    encode = toInteger
    decode = fromInteger
