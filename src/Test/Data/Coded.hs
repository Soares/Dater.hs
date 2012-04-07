{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Coded where
import Data.Coded
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testCoded :: forall a. (Show a, Arbitrary a, Eq a, Coded a) => a -> Test
testCoded _ = testGroup "encoding & decoding"
    [ testProperty "encode→decode" (\i -> encode (decode i :: a) == i)
    , testProperty "decode→encode" (\(a::a) -> decode (encode a) == a)
    ]
