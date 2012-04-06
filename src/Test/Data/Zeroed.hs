{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Zeroed where
import Data.Zeroed
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testZeroed :: Test
testZeroed = testGroup "Zeroed"
    [ testProperty "predecessors"
        (forAll (choose (-128, 127)) (\i -> length (predecessors i) == abs i))
    ]
