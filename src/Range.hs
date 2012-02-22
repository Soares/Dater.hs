{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Range where
import Control.Arrow
import Prelude hiding (Enum(..))
import FullEnum

class Enum a => Range x a | a -> x where
    range :: x -> (a, a)
    range = (start &&& end)
    start :: x -> a
    start = fst . range
    end :: x -> a
    end = snd . range
