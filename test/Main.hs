{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Framework

import Data.Calendar.Gregorian
-- import Data.Calendar.Kaol
import Data.Calendar.Tests
import Data.Naturals.Tests
import Data.Modable.Tests
import Data.Pair.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Gregorian" [testCalendar  (undefined::Gregorian)]
    -- , testGroup "Kaol" [testCalendar (undefined::Kaol)]
    -- TODO: Turn Kaol back on
    , testModable (undefined::Integer)
    , testNaturals
    , testPairs
    ]
