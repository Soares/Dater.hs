import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.DateTime.Gregorian
import Data.Naturals

main = defaultMain tests

tests =
    [ testGroup "YMD"
        [ testProperty "Indempotent Year"  (propEnumIndempotence :: Year  -> Bool)
        , testProperty "Indempotent Month" (propEnumIndempotence :: Month -> Bool)
        , testProperty "Indempotent Day"   (propEnumIndempotence :: Day   -> Bool)
        , testProperty "Indempotent YMD"   (propEnumIndempotence :: YMD   -> Bool)
        ]
    , testGroup "HMS"
        [ testProperty "Indempotent Hour"   (propEnumIndempotence :: N24 -> Bool)
        , testProperty "Indempotent Minute" (propEnumIndempotence :: N60 -> Bool)
        , testProperty "Indempotent HMS"    (propEnumIndempotence :: HMS -> Bool)
        ]
    ]

propEnumIndempotence :: (Enum a, Eq a) => a -> Bool
propEnumIndempotence a = succ (pred a) == a
