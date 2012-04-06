module Properties.Pair where
import Pair
import Test.QuickCheck

propInversion :: (Eq (p a b), Pair p) => p a b -> Bool
propInversion p = p == fromTuple (toTuple p)
