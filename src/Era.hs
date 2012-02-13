{-# LANGUAGE TemplateHaskell #-}
module Era where
import Data.Function ( on )
import Data.Label
import Earthlike
import Prelude hiding ((.), id)

data Calendar :: String

data Era = Era
    { _name      :: String
    , _format    :: EarthlikeFormat
    , _beginning :: Rational
    , _codes     :: String
    -- TODO: there is no year zero. There is 1BC and 1AD. Allow for it.
    , _precodes  :: String
    }
mkLabels [''Era]

instance Show Era where show = get name
instance Eq Era where (==) = (==) `on` get name
instance Ord Era where (<=) = (<=) `on` get beginning
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
