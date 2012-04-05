module Pair
    ( Pair(..)
    , left
    , right
    , tmap
    , summarize
    , merge
    , both
    ) where

-- | Things that can act like a two-tuple
class Pair t where
    toTuple :: t a b -> (a, b)
    fromTuple :: (a, b) -> t a b

-- | The simple tuple instance
instance Pair (,) where
    toTuple = id
    fromTuple = id

-- | Pair's version of 'fst'
left :: Pair t => t a b -> a
left = fst . toTuple

-- | Pair's version of 'snd'
right :: Pair t => t a b -> b
right = snd . toTuple

-- | Map two functions across the pair, the first one
-- | being applied to the left, the second to the right
tmap :: Pair t => (a -> x) -> (b -> y) -> t a b -> t x y
tmap f g ab = fromTuple (f $ left ab, g $ right ab)

-- | Map one function (of two parameters) across the pair,
-- | generating a summary value
summarize :: Pair t => (a -> b -> x) -> t a b -> x
summarize f ab = f (left ab) (right ab)

-- | Merge a pair (of functions) with a pair (of parameters)
-- | generating a new pair
merge :: Pair t => t (f -> x) (g -> y) -> t f g -> t x y
merge fn xs = fromTuple (left fn $ left xs, right fn $ right xs)

-- | Apply one function to both elements of a homogenous pair
both :: Pair t => (a -> b) -> t a a -> t b b
both f = tmap f f
