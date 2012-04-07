{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Modable.Tests where
import Data.Maybe (isNothing)
import Data.Modable
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testModable :: forall a b.
    ( Eq a, Show a, Arbitrary a
    , Eq (Relative a), Show (Relative a), Arbitrary (Relative a)
    , Relative a ~ Maybe b
    , Modable a
    ) => a -> Test
testModable _ = testGroup "maybe math"
    [ testProperty "plus→minus" (\(a::a) (b::Relative a) ->
        minus (plus a b) b == a)
    , testProperty "minus→plus" (\(a::a) (b::Relative a) ->
        plus (minus a b) b == a)
    , testProperty "clobber" (\(a::a) (b::Relative a) ->
        if isNothing b
            then clobber a b == a
            else clobber a b `like` b)
    , testProperty "absify relify" (\(a::a) -> absify (relify a) == Just a)
    , testProperty "like" (\(a::a) -> a `like` relify a)
    ]
