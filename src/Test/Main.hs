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
import Test.Data.Normalize
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
        ]
    , testGroup "Naturals"
        [ testEnum (undefined::N1)
        , testEnum (undefined::N24)
        , testEnum (undefined::N60)
        , testEnum (undefined::N100)
        , testEnum (undefined::N256)
        ]
    , testGroup "HMS"
        [ testEnum (undefined::HMS)
        , testCoded (undefined::HMS)
        , testModable (undefined::HMS)
        , testNormal (undefined::YMD)
        ]
    , testZeroed
    ]
