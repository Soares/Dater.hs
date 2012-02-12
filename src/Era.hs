{-# LANGUAGE TemplateHaskell #-}
module Era where
import Data.Function ( on )
import Data.Label
import Earthlike
import Prelude hiding ((.), id)

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
