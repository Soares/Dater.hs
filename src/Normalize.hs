module Normalize (Normalize(..)) where

-- | This class is for values that can be entered (and persisted) in
-- | a non-normal format for a time, but eventually need to be normalized.
-- | The steriotypical example is a 'time' value, where the user can
-- | enter '123 seconds' (or '61 seconds + 62 seconds'), but the value
-- | is normalized to '2 minutes, 3 seconds' before display.
-- | Note that normalization should not change the *meaning* of the value,
-- | but should rather simplfy it to it's most reduced form.
class Normalize a where
    normalize :: a -> a
    isNormal :: a -> Bool
    overflow :: a -> Int
    overflow = const 0
    underflow :: a -> Int
    underflow = const 0

instance Normalize Integer where
    normalize = id
    isNormal = const True

instance Normalize Int where
    normalize = id
    isNormal = const True
