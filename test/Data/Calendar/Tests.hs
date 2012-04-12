{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
--TODO: Remove
{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Tests where
import Data.Calendar
-- import Data.Enum.Tests
import Data.Modable
-- import Data.Modable.Tests
-- import Data.Normalize
-- import Data.Normalize.Tests
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
-- TODO: Remove
import Data.Calendar.Gregorian.Date
import Data.Calendar.Gregorian.Time

type Behaved x r = (Arbitrary x, Arbitrary r, Relative x ~ Maybe r)

testCalendar :: forall dt z r.
    ( CalendarLike dt z
    , Behaved dt r
    , ZoneLike z
    ) => Calendar dt z -> Test
testCalendar _ = testGroup "Calendar"
    -- [ testEnum (undefined::dt)
    -- , testModable (undefined::dt)
    -- , testNormal (undefined::dt)
    -- , testProperty "from→to Integer" (\i -> toInteger (fromInteger i :: dt) == i)
    -- TODO: test that normal frominteger is equal to frominteger
    [ testProperty "to→from Integer" (\(x::(Year:::Month:/:Day:::Hour:::Minute:/:Second)) -> fromInteger (toInteger x) == x)
    -- , testProperty "no overflow" (\(x::dt) -> overflow x == 0)
    ]
