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

import qualified Data.DateTime.Gregorian as Gregorian
import qualified Data.DateTime.Kaol as Kaol
import Data.Naturals
import Data.Normalize

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Gregorian YMD" [testVarPart (undefined::Gregorian.YMD)]
    , testGroup "Gregorian HMS" [testConstPart (undefined::Gregorian.HMS)]
    , testGroup "Kaol YMD" [testVarPart (undefined::Kaol.YMD)]
    , testGroup "Kaol HMS" [testConstPart (undefined::Kaol.HMS)]
    , testNaturals
    , testZeroed
    ]
