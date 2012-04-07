{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DateTime.Tests where
import Data.Coded.Tests
import Data.DateTime
import Data.Enum.Tests
import Data.Modable
import Data.Modable.Tests
import Data.Normalize
import Data.Normalize.Tests
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

type Behaved x r = (Arbitrary x, Arbitrary r, Relative x ~ Maybe r)

testDate :: forall d r. (DateLike d, Behaved d r) => d -> Test
testDate d = testGroup "date composition"
    [ testEnum d
    , testCoded d
    , testModable d
    , testNormal d
    , testProperty "no overflow" (\(x::d) -> overflow x == 0)
    ]

testTime :: forall t r. (TimeLike t, Behaved t r) => t -> Test
testTime t = testGroup "time composition"
    [ testEnum t
    , testCoded t
    , testModable t
    , testNormal t
    , testProperty "reconstructible" (\(x::t) ->
        let (o, y) = normalize x
            size = succ (maxBound - minBound :: t)
            p = o * fromIntegral size
        in (p + fromIntegral y) == fromIntegral x)
    ]

testDateTime :: forall d t r s.
    ( DateLike d, Behaved d r
    , TimeLike t, Behaved t s
    ) => DateTime d t -> Test
testDateTime _ = testGroup "DateTime"
    [ testDate (undefined::d)
    , testTime (undefined::t)
    ]
