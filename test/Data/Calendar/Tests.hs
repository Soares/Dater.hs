{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Calendar.Tests where
import Data.Calendar
import Data.Enum.Tests
import Data.Normalize
import Data.Normalize.Tests
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testCalendar :: forall dt z.
    ( CalendarLike dt z
    , Arbitrary dt
    , ZoneLike z
    ) => Calendar dt z -> Test
testCalendar _ = testGroup "Calendar"
    [ testEnum (undefined::dt)
    , testNormal (undefined::dt)
    , testProperty "to→from Integer" (\(x::dt) -> fromInteger (toInteger x) == x)
    , testProperty "from→to Integer" (\i -> toInteger (fromInteger i::dt) == i)
    , testProperty "normalized read" (\i -> isNormal (fromInteger i::dt))
    , testProperty "no overflow" (\(x::dt) -> overflow x == 0)
    ]
