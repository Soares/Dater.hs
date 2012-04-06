module Data.Coded (Coded(..)) where

-- | Things that can be encoded and decoded
class Coded a where
    encode :: a -> Integer
    decode :: Integer -> a
