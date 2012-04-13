{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Modable where
import Control.Applicative
import Data.Calendar.Utils
import Data.Maybe (fromMaybe, isJust)
import Data.Pair
import Prelude hiding (fst, snd, curry, uncurry)

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

isAbsolute :: forall a. Modable a => Relative a -> Bool
isAbsolute = isJust . (absify :: Relative a -> Maybe a)

instance (Relable a, Relable b, Pair (-&)) => Relable (a -& b) where
    type Relative (a -& b) = (Relative a -& Relative b)

instance (Modable a, Modable b, Pair (-&)) => Modable (a -& b) where
    plus = merge . (plus *** plus)
    minus = merge . (minus *** minus)
    clobber = merge . (clobber *** clobber)
    like = uncurry (&&) .: (merge . (like *** like))
    relify = relify *** relify
    absify = join . (absify *** absify) where
        join p = build <$> fst p <*> snd p


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
