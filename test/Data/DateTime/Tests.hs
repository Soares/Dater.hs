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

testDateTime :: forall dt z r.
    ( DateTimeLike dt z
    , Behaved dt r
    , ZoneLike z
    ) => DateTime dt z -> Test
testDateTime _ = testGroup "DateTime"
    [ testEnum (undefined::dt)
    , testCoded (undefined::dt)
    , testModable (undefined::dt)
    , testNormal (undefined::dt)
    , testProperty "no overflow" (\(x::dt) -> overflow x == 0)
    ]
