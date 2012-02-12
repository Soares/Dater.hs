module Dater where
{-
import Data.Label

-- | Dates are stored as rationals. These rationals track the number of
-- | major units since a fixed point in time.
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
    { _value     :: Rational
    , _beginning :: Rational
    } deriving (Eq, Show)
mkLabels [''Date]

data Format = Format
    { _beginning     :: Rational
    , _unitsInDay    :: Integer
    } deriving (Eq, Show)
mkLabels [''Format]


instance Ord Format where
    x <= y = norm x <= norm y where
        norm z = beginning z / toRational (unitsInDay z)


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
-}
