{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Pair.Tests where
import Data.DateTime ((:::))
import Data.Pair
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testPair :: forall p a b.
    ( Eq (p a b)
    , Show (p a b)
    , Arbitrary (p a b)
    , Pair p) => p a b -> Test
testPair _ = testGroup "pair"
    [ testProperty "indempotence" (\(p::p a b) -> fromTuple (toTuple p) == p)
    , testProperty "rebuild" (\(p::p a b) -> build (left p) (right p) == p)
    ]

testPairs :: Test
testPairs = testGroup "Pairs"
    [ testPair (undefined :: (Int, Int))
    , testPair (undefined :: (Int:::Int))
    ]
