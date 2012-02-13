{-# LANGUAGE ExistentialQuantification #-}
module Date
    ( Date(Date)
    , normalize
    , convert
    , add
    , sub
    ) where
import Calendar
import Control.Category ((.))
import Data.Function ( on )
import Prelude hiding ((.), id)

-- | Dates are stored as rationals in a calendar.
-- | Calendars are used to convert dates into a normalized form.
-- | Dates can only be compared insofar as their calendars are
-- | normalizing against the same starting point.
-- |
-- | See the Calendar data type for notes on normalization.
data Date = forall c. Calendar c => Date c Rational

instance Show Date where show (Date c v) = display c v
instance Eq Date where (==) = (==) `on` unwrap
instance Ord Date where (<=) = (<=) `on` unwrap

wrap :: Calendar c => c -> Rational -> Date
wrap c r = Date c (denormalize c r)

unwrap :: Date -> Rational
unwrap (Date c v) = normalize c v


convert :: Calendar c => c -> Date -> Date
convert c = wrap c . unwrap


combine :: (Rational -> Rational -> Rational) -> Date -> Date -> Date
combine op x@(Date c _) y = wrap c (unwrap x `op` unwrap y)

add, sub :: Date -> Date -> Date
add = combine (+)
sub = combine (-)
