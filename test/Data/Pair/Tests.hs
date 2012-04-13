{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Pair.Tests where
import Data.Pair
import Prelude hiding (fst, snd, curry, uncurry)
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
    , testProperty "rebuild" (\(p::p a b) -> build (fst p) (snd p) == p)
    -- TODO: Many more pair tests
    ]

testPairs :: Test
testPairs = testGroup "Pairs" [ testPair (undefined :: (Int, Int)) ]
