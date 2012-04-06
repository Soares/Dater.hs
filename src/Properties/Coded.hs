{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Coded where
import Test.QuickCheck
import Coded

propIndempotence :: forall a. Coded a => a -> Integer -> Bool
propIndempotence _ i = i == encode (decode i :: a)

propIndempotence2 :: forall a. (Eq a, Coded a) => a -> Bool
propIndempotence2 a = a == (decode (encode a) :: a)
