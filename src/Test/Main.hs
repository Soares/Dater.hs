{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.DateTime.VarPart
import Data.DateTime.ConstPart


import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Data.Coded
import Test.Data.Enum
import Test.Data.Modable
import Test.Data.Naturals
import Test.Data.Normalize
import Test.Data.Pair
import Test.Data.Zeroed

import Data.DateTime.Gregorian
import Data.Naturals
import Data.Normalize

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "YMD"
        [ testEnum (undefined::YMD)
        , testCoded (undefined::YMD)
        , testModable (undefined::YMD)
        , testNormal (undefined::YMD)
        , testProperty "no overflow" (\(a::YMD) -> fst (normalize a) == 0)
        , testPair (undefined::YMD)
        ]
    , testGroup "HMS"
        [ testEnum (undefined::HMS)
        , testCoded (undefined::HMS)
        , testModable (undefined::HMS)
        , testNormal (undefined::HMS)
        , testPair (undefined::HMS)
        , testProperty "reconstructible" (\(a::HMS) ->
            let (o, b) = normalize a
                size = succ (maxBound - minBound :: HMS)
                x = o * fromIntegral size
            in (x + fromIntegral b) == fromIntegral a)
        ]
    , testNaturals
    , testZeroed
    ]
