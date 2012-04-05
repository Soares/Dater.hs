module TwoTuple
    ( TwoTuple(..)
    , left
    , right
    , tmap
    , summarize
    , merge
    , both
    ) where

class TwoTuple t where
    toTuple :: t a b -> (a, b)
    fromTuple :: (a, b) -> t a b

instance TwoTuple (,) where
    toTuple = id
    fromTuple = id

left :: TwoTuple t => t a b -> a
left = fst . toTuple

right :: TwoTuple t => t a b -> b
right = snd . toTuple

tmap :: TwoTuple t => (a -> x) -> (b -> y) -> t a b -> t x y
tmap f g ab = fromTuple (f $ left ab, g $ right ab)

summarize :: TwoTuple t => (a -> b -> x) -> t a b -> x
summarize f ab = f (left ab) (right ab)

merge :: TwoTuple t => t (f -> x) (g -> y) -> t f g -> t x y
merge fn xs = fromTuple (left fn $ left xs, right fn $ right xs)

both :: TwoTuple t => (a -> a) -> t a a -> t a a
both f = tmap f f
