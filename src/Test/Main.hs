{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.DateTime.VarPart
import Data.DateTime.ConstPart

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Data.DateTime.ConstPart
import Test.Data.DateTime.VarPart
import Test.Data.Naturals
import Test.Data.Zeroed

import Data.DateTime.Gregorian
import Data.Naturals
import Data.Normalize

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "YMD" [testVarPart (undefined::YMD)]
    , testGroup "HMS" [testConstPart (undefined::HMS)]
    , testNaturals
    , testZeroed
    ]
