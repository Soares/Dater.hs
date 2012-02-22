module FullEnum where
import Prelude hiding (Enum(..))

class Enum a where
    -- | the successor of a value.  For numeric types, 'succ' adds 1.
    succ                :: a -> a
    -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
    pred                :: a -> a
    -- | Convert from an 'Integer'.
    toEnum              :: Integer -> a
    -- | Convert to an 'Integer'.
    fromEnum            :: a -> Integer

    -- | Used in Haskell's translation of @[n..]@.
    enumFrom            :: a -> [a]
    -- | Used in Haskell's translation of @[n,n'..]@.
    enumFromThen        :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n..m]@.
    enumFromTo          :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n,n'..m]@.
    enumFromThenTo      :: a -> a -> a -> [a]

    succ                   = toEnum . (+) 1  . fromEnum
    pred                   = toEnum . (-) 1 . fromEnum
    enumFrom x             = map toEnum (enumFrom (fromEnum x))
    enumFromThen x y       = map toEnum (enumFromThen (fromEnum x) (fromEnum y))
    enumFromTo x y         = map toEnum (enumFromTo (fromEnum x) (fromEnum y))
    enumFromThenTo x1 x2 y = map toEnum (enumFromThenTo (fromEnum x1) (fromEnum x2) (fromEnum y))

instance Enum Integer where
    toEnum = id
    fromEnum = id
    succ = (+1)
    pred = (+1)
    enumFrom x = x : enumFrom (x+1)
    enumFromThen x y = e' x (y-x) where
        e' x d = x : e' (x+d) d
    enumFromTo x y
        | abs x <= abs y = x : enumFromTo (x+1) y
        | otherwise = []
    enumFromThenTo x1 x2 y = e' x1 (x2-x1) y where
        e' x d y
            | abs x <= abs y = x : e' (x+d) d y
            | otherwise = []
instance Enum Int where
    toEnum = fromIntegral
    fromEnum = fromIntegral
