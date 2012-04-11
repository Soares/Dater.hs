module Data.Normalize (Normalize(..), like) where
import Control.Arrow

-- | This class is for values that can be entered (and persisted) in
-- | a non-normal format for a time, but eventually need to be normalized.
-- | The steriotypical example is a 'time' value, where the user can
-- | enter '123 seconds' (or '61 seconds + 62 seconds'), but the value
-- | is normalized to '2 minutes, 3 seconds' before display.
-- | Note that normalization should not change the *meaning* of the value,
-- | but should rather simplfy it to it's most reduced form.
class Normalize a where
    normalize :: a -> (Int, a)
    normalize = overflow &&& normal
    normal :: a -> a
    normal = snd . normalize
    overflow :: a -> Int
    overflow = fst . normalize
    isNormal :: a -> Bool

instance Normalize Integer where
    normalize = const 0 &&& id
    isNormal = const True

instance Normalize Int where
    normalize = const 0 &&& id
    isNormal = const True

like :: (Eq a, Normalize a) => a -> a -> Bool
a `like` b = normal a == normal b
