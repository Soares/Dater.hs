{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Range
    ( Ranged(..)
    , elements
    , count
    ) where
import Control.Applicative
import Control.Arrow
import Zeroed

class Enum a => Ranged a x | a -> x, x -> a where
    range :: x -> (a, a)
    range = start &&& end
    start :: x -> a
    start = fst . range
    end :: x -> a
    end = snd . range

elements :: Ranged a x => x -> [a]
elements = enumFromTo <$> start <*> end

count :: (Num a, Ranged a x) => x -> a
count x = end x - start x
