module Data.Pair
    ( Pair(..)
    , left
    , right
    , tmap
    , summarize
    , merge
    , both
    ) where

-- | Things that can act like a two-tuple
class Pair p where
    toTuple :: p a b -> (a, b)
    fromTuple :: (a, b) -> p a b
    fromTuple = uncurry build
    build :: a -> b -> p a b
    build = (fromTuple .) . (,)

-- | The simple tuple instance
instance Pair (,) where
    toTuple = id
    fromTuple = id

-- | Pair's version of 'fst'
left :: Pair p => p a b -> a
left = fst . toTuple

-- | Pair's version of 'snd'
right :: Pair p => p a b -> b
right = snd . toTuple

-- | Map two functions across the pair, the first one
-- | being applied to the left, the second to the right
tmap :: (Pair p, Pair q) => (a -> x) -> (b -> y) -> p a b -> q x y
tmap f g ab = fromTuple (f $ left ab, g $ right ab)

-- | Map one function (of two parameters) across the pair,
-- | generating a summary value
summarize :: Pair p => (a -> b -> x) -> p a b -> x
summarize f ab = f (left ab) (right ab)

-- | Merge a pair (of functions) with a pair (of parameters)
-- | generating a new pair
merge :: (Pair p, Pair q) => p (f -> x) (g -> y) -> p f g -> q x y
merge fn xs = fromTuple (left fn $ left xs, right fn $ right xs)

-- | Apply one function to both elements of a homogenous pair
both :: (Pair p, Pair q) => (a -> b) -> p a a -> q b b
both f = tmap f f
