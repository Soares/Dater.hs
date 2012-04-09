{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Framework

import Data.DateTime.Gregorian
import Data.DateTime.Kaol
import Data.DateTime.Tests
import Data.Naturals.Tests
import Data.Pair.Tests
import Data.Zeroed.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Gregorian" [testDateTime  (undefined::Gregorian)]
    -- , testGroup "Kaol" [testDateTime (undefined::Kaol)]
    -- TODO: Turn Kaol back on
    , testNaturals
    , testPairs
    , testZeroed
    ]
