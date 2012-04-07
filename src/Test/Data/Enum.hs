{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Enum where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testEnum :: forall a. (Show a, Arbitrary a, Enum a, Eq a) => a -> Test
testEnum _ = testGroup "enumerability"
    [ testProperty "succ→pred" (\(a::a) -> succ (pred a) == a)
    , testProperty "pred→succ" (\(a::a) -> pred (succ a) == a)
    ]
