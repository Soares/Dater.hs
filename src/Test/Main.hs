import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.DateTime.Gregorian

main = defaultMain tests

tests =
    [ testGroup "YMD"
        [ testProperty "Indempotent Year"  (propEnumIndempotence :: Year  -> Bool)
        , testProperty "Indempotent Month" (propEnumIndempotence :: Month -> Bool)
        , testProperty "Indempotent Day"   (propEnumIndempotence :: Day   -> Bool)
        , testProperty "Indempotent YMD"   (propEnumIndempotence :: YMD   -> Bool)
        ]
    , testGroup "HMS"
        [
        ]
    ]

propEnumIndempotence :: (Enum a, Eq a) => a -> Bool
propEnumIndempotence a = succ (pred a) == a
