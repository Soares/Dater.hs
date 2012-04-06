{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Modable where
import Data.Pair
import Data.Maybe (fromMaybe)

-- | A class that allows simple 'arithmetic' betwene
-- | different types. In simplet types, 'b' is 'Maybe a'
-- | which allows us to do addition between (for instance)
-- | Integers and Maybe Integers.
-- |
-- | This class is motivated by the need for adding dates
-- | as follows:
-- |
-- | 2012/1/1 `plus` /1/30
-- |
-- | Which adds [Nothing Years, Just 1 Month, Just 30 Days] to
-- | an existing date.
class Modable a b where
    plus :: a -> b -> a
    minus :: a -> b -> a
    -- b replaces a, if b is given (and can be converted to a)
    clobber :: a -> b -> a


-- | Helper function for modable pairs
thread :: Pair p
    => (a -> x -> a) -> (b -> y -> b)   -- The modifying functions
    -> p a b -> Maybe (p x y)       -- The pair parameters
    -> p a b                          -- The resulting pair
thread f g ab = maybe ab (merge $ tmap f g ab)


-- | Instances that are modable on Maybe objects
-- | There's a little type system hackery going on here,
-- | so be careful how you make new instances.

instance (Modable a x, Modable b y, Pair p)
    => Modable (p a b) (Maybe (p x y)) where
    plus = thread plus plus
    minus = thread minus minus
    clobber = thread clobber clobber

instance Num a => Modable a (Maybe a) where
    plus a = maybe a (a+)
    minus a = maybe a (a-)
    clobber = fromMaybe
