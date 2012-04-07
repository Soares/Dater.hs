{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Normalize where
import Data.Normalize
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testNormal :: forall a. (Show a, Arbitrary a, Eq a, Normalize a) => a -> Test
testNormal _ = testGroup "normalization"
    [ testProperty "isNormal ⇒ no change" (\(a::a) ->
        if isNormal a then normal a == a else True)
    ]
