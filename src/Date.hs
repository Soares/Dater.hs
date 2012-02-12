{-# LANGUAGE TemplateHaskell #-}
module Date where
import Control.Category ((.), id)
import Data.Function ( on )
import Data.Label
import Prelude hiding ((.), id)

-- | Dates are stored as rationals in an era.
-- | These rationals keep track of days since a relative beginning.
-- | The era contains a beginning since the 'all-beginning'.
-- | Obviously, this date system only works where there exists:
-- |
-- | * One relatively-fixed point in time (a 'beginning')
-- | * Constant time flow between all comparable dates
-- | * A major unit comparable between all times (a 'day')
-- |
-- | Thus, this date system works well for single-world systems constrained
-- | to non-relativistic speeds, but may run into trouble with advaced sci-fi.
-- |
-- | The 'beginning' and 'day' values are kept in the 'Format' datatype.
-- |
-- | Note that only dates based on the same major unit (i.e. dates based
-- | on 'days') can be compared. In earthlike systems, 'days' are the best
-- | choice of base unit because many older calendars didn't understand the
-- | subtleties of years (i.e. pre-Gregorian calendars), and got off-synch
-- | over time. However, most dates share the basis of planetary revolution.
data Date = Date
    { _value :: Rational
    , _era   :: Era
    } deriving (Eq, Show)
mkLabels [''Date]

instance Ord Date where (<=) = (<=) `on` normalize


normalize :: Date -> Rational
normalize d = get value d + get (beginning . era) d


wrap :: Rational -> Era -> Date
wrap r e = Date (r - get beginning e) e


convert :: Date -> Era -> Date
convert d e = wrap (normalize d) e


combine :: (Rational -> Rational) -> Date -> Date -> Date
combine op x y = wrap (normalize x `op` normalize y) (get era x)
add, sub :: Date -> Date -> Date
add = combine (+)
sub = combine (-)

sub x y = wrap (normalize x - normalize y) (get era x)
neg :: Date -> Date
neg = modify negate value



-- | Time from the beginning, measured in days
unit :: Format
unit = Format 0 1


-- | The number of days since the beginning of this date format
start :: Format -> Rational
start = days 0


-- | Convert a rational to the number of days since the beginning
normalize :: Rational -> Format -> Rational
normalize r x = (r + get beginning x) / toRational (get unitsInDay x)


instance Ord Date where
    x <= y = get value x <= get value (y `like` x)


instance Num Date where
    x + y = modify value ((+) $ get value $ y `like` x) x
    x * y = modify value ((*) $ get value $ y `like` x) x
    negate = modify value negate
    abs = modify value abs
    signum = modify value signum
    fromInteger i = Date (toRational i) unit


convert :: Date -> Format -> Date
convert (Date r x) y = Date r' y where
    r' = normalize r x * toRational (unitsInDay y)


like :: Date -> Date -> Date
a `like` b = convert a $ get format b
