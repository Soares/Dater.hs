{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Modable where
import Control.Applicative
import Data.Pair
import Data.Maybe (fromMaybe)

class Relable a where type Relative a

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
class Relable a => Modable a where
    plus :: a -> Relative a -> a
    minus :: a -> Relative a -> a
    -- b replaces a, if b is capeable
    clobber :: a -> Relative a -> a
    relify :: a -> Relative a
    absify :: Relative a -> Maybe a
    like :: a -> Relative a -> Bool


-- | Helper function for modable pairs
thread :: (Pair p, Pair q, Relable a, Relable b)
    => (a -> Relative a -> a) -> (b -> Relative b -> b)
    -> p a b -> Maybe (q (Relative a) (Relative b))
    -> p a b
thread f g ab = maybe ab (merge $ tmap f g ab)

instance (Relable a, Relable b, Pair p) => Relable (p a b) where
    type Relative (p a b) = Maybe ((Relative a), (Relative b))

instance (Modable a, Modable b, Pair p) => Modable (p a b) where
    plus = thread plus plus
    minus = thread minus minus
    clobber = thread clobber clobber
    like _ Nothing = True
    like p (Just q) = (left p `like` left q) && (right p `like` right q)
    relify p = Just (build (relify $ left p) (relify $ right p))
    absify = (join =<<) where
        join p = build <$> (absify $ left p) <*> (absify $ right p)


instance Relable Integer where type Relative Integer = Maybe Integer
instance Modable Integer where
    plus a = maybe a (a+)
    minus a = maybe a (a-)
    clobber = fromMaybe
    like a = maybe True (a ==)
    absify = id
    relify = pure

instance Relable Int where type Relative Int = Maybe Int
instance Modable Int where
    plus a = maybe a (a+)
    minus a = maybe a (a-)
    clobber = fromMaybe
    like a = maybe True (a ==)
    absify = id
    relify = pure
