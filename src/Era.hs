{-# LANGUAGE TemplateHaskell #-}
module Era where
import Control.Category ((.), id)
import Data.Function ( on )
import Data.Label
import Prelude hiding ((.), id)

data Era = Era
    { _name      :: String
    , _beginning :: Rational
    , _codes     :: String
    , _precodes  :: String
    } deriving (Eq, Show)
mkLabels [''Era]

instance Ord Era where (<=) = (<=) `on` get beginning
