{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.DateTime.VarPart
import Data.DateTime.ConstPart

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.DateTime.Gregorian
import Data.DateTime.Kaol
import Data.DateTime.Tests
import Data.Naturals.Tests
import Data.Normalize.Tests
import Data.Pair.Tests
import Data.Zeroed.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Gregorian" [testDateTime  (undefined::Gregorian)]
    , testGroup "Kaol" [testDateTime (undefined::Kaol)]
    , testNaturals
    , testPairs
    , testZeroed
    ]
