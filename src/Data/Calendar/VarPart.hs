{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Calendar.VarPart ((:/:)(..)) where
import Control.Applicative
import Control.Arrow ((***))
import Data.Normalize
import Data.Pair
import Data.Ranged
import Test.QuickCheck.Arbitrary
import Text.Format.Write

-- | A simple combinator, intended for combining types into a date type
data a :/: b = a :/: b deriving (Eq, Ord)

-- | Some utilities to treat :/: like a tuple
instance Pair (:/:) where
    toTuple (a:/:b) = (a,b)
    fromTuple (a,b) = a:/:b

-- | The slash is utalized by default when showing date values.
-- | Use a newtype and override for different semantics.
-- | Note that you can easily change the formatting with a formatter
-- | without resorting to newtypes.
instance (Show a, Show b) => Show (a:/:b) where
    show (a:/:b) = show a ++ "/" ++ show b

-- | Allows us to generate prior and succeeding dates
instance (Normalize a, Integral a, Integral b, Ranged b a) => Enum (a:/:b) where
    succ (a:/:b)
        | not (isNormal (a:/:b)) = succ (normal (a:/:b))
        | b < end a = a :/: succ b
        | otherwise = succ a :/: start (succ a)
    pred (a:/:b)
        | not (isNormal (a:/:b)) = pred (normal (a:/:b))
        | b > start a = a :/: pred b
        | otherwise = pred a :/: end (pred a)
    toEnum = fromIntegral
    fromEnum = fromIntegral

-- | Normalizes the date such that all parts are within their respective
-- | ranges. By convention, dates shouldn't overflow: they should just get
-- | bigger and smaller.
instance
    ( Normalize a, Integral a, Integral b, Ranged b a
    ) => Normalize (a:/:b) where
    isNormal (a:/:b) = isNormal a && isInRange a b
    normalize (a:/:b)
        | not (isNormal a) = let
            (o, a') = normalize a
            (p, ab) = normalize $ a' :/: b
            in (o+p, ab)
        | isInRange a b = (0, a:/:b)
        | b > end a = let
            a' = succ a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a
            in normalize $ a' :/: (adjusted - delta)
        | otherwise = let
            a' = pred a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a'
            in normalize $ a' :/: (adjusted + delta)

instance (Integral a, Integral b, Ranged b a) => Num (a:/:b) where
    (a:/:b) + (x:/:y) = (a+x) :/: (b+y)
    (a:/:b) - (x:/:y) = (a-x) :/: (b-y)
    (a:/:b) * (x:/:y) = (a*x) :/: (b*y)
    negate (a:/:b) = negate a :/: b
    abs (a:/:b) = abs a :/: abs b
    signum (0:/:0) = 0
    signum (a:/:b) = if a < 0 then -1 else 1
    fromInteger = fromTuple . split

-- TODO: Remove composable (globally)
instance (Normalize a, Integral a, Integral b, Ranged b a) => Real (a:/:b) where
    toRational = fromIntegral . toInteger

instance
    ( Integral a, Normalize a, Integral b, Ranged b a
    ) => Integral (a:/:b) where
    toInteger = (size <$> left <*> right) . normal
    quotRem a x = (fromInteger *** fromInteger) $
        (toInteger a) `quotRem` (toInteger x)

-- | When asked to be formatted, give the number of days since 0
instance
    ( Normalize a, Integral a, Integral b, Ranged b a
    ) => WriteBlock (a:/:b) where
    numerical = show . toInteger

-- | Allows QuickCheck testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (a:/:b) where
    arbitrary = (:/:) <$> arbitrary <*> arbitrary
    -- This could be `shrink = normal`, if we didn't mind some trouble
    -- debugging the normalization code...
    shrink (a:/:b) = [ x:/:b | x <- shrink a ] ++ [ a:/:y | y <- shrink b ]
