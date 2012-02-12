module Format
    ( Format
    , beginning
    , unitsInDay
    , unit
    , normalize
    , days
    ) where
import Data.Label
import Category ((.), id)
import Prelude hiding ((.), id)

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
