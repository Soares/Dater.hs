{-# LANGUAGE TemplateHaskell #-}
module Date
    ( Date(Date)
    , era
    , value
    , normalize
    , convert
    , add
    , sub
    ) where
import Control.Category ((.))
import Data.Function ( on )
import Data.Label
import Era
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
    { _era   :: Era
    , _value :: Rational
    } deriving (Eq, Show)
mkLabels [''Date]

instance Ord Date where (<=) = (<=) `on` normalize


normalize :: Date -> Rational
normalize d = get value d + get (beginning . era) d


wrap :: Era -> Rational -> Date
wrap e r = Date e (r - get beginning e)

unwrap :: Date -> Rational
unwrap d = get value d + get (beginning . era) d


convert :: Era -> Date -> Date
convert e = wrap e . normalize


combine :: (Rational -> Rational -> Rational) -> Date -> Date -> Date
combine op x y = wrap (get era x) (normalize x `op` normalize y)
add, sub :: Date -> Date -> Date
add = combine (+)
sub = combine (-)
