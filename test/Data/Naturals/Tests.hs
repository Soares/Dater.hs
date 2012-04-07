{-# LANGUAGE ScopedTypeVariables #-}
module Data.Naturals.Tests where
import Data.Naturals
import Data.Normalize
import Data.Enum.Tests
import Test.Framework
import Test.Framework.Providers.QuickCheck2

testNaturals :: Test
testNaturals = testGroup "Naturals"
    [ testEnum (undefined::N1)
    , testEnum (undefined::N200)
    , testProperty "normal +" (\(n::N10) (m::N10) ->
        normal (n + m) == normal (normal n + normal m))
    , testProperty "normal -" (\(n::N10) (m::N10) ->
        normal (n - m) == normal (normal n - normal m))
    , testProperty "normal *" (\(n::N10) (m::N10) ->
        normal (n * m) == normal (normal n * normal m))
    ]
