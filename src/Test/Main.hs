{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.DateTime.VarPart
import Data.DateTime.ConstPart

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Data.DateTime
import Test.Data.Naturals
import Test.Data.Pair
import Test.Data.Zeroed

import Data.DateTime.Gregorian
import Data.DateTime.Kaol
import Data.Naturals
import Data.Normalize

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
