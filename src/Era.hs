{-# LANGUAGE TemplateHaskell #-}
module Era where
import Data.Function ( on )
import Data.Label
import Prelude hiding ((.), id)

data Era = Era
    { _name      :: String
    , _beginning :: Rational
    , _codes     :: String
    -- TODO: there is no year zero. There is 1BC and 1AD. Allow for it.
    , _precodes  :: String
    } deriving (Eq, Show)
mkLabels [''Era]

instance Ord Era where (<=) = (<=) `on` get beginning
