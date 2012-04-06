import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Data.Enum
import Test.Data.Coded

import Data.DateTime.Gregorian
import Data.Naturals

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "YMD"
        [ testEnum (undefined::Year)
        , testEnum (undefined::Month)
        , testEnum (undefined::Day)
        , testEnum (undefined::YMD)
        , testCoded (undefined::YMD)
        ]
    , testGroup "HMS"
        [ testEnum (undefined::N24)
        , testEnum (undefined::N60)
        , testEnum (undefined::HMS)
        , testCoded (undefined::HMS)
        ]
    ]
