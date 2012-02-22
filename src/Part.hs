{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Part where
import Data.List.Zipper
import Control.Applicative
import Alter
import Possibilities
import Threadable
import Total
import Prelude hiding (length)

data a :/: b = a :/: b deriving (Eq, Ord, Show)

instance
    ( Threadable a x a
    , Threadable b a ()
    ) => Threadable (a:/:b) x () where
    thread x = let
        (a, a') = thread x
        (b, ()) = thread a'
        in (a:/:b, ())
    unthread (a:/:b) () = unthread a (unthread b ())

instance
    ( Possibilities x a
    , Possibilities (x:/:a) b
    ) => Possibilities x (a:/:b) where
    possibilities x = possibilities x >>= merge where
        merge a = (a:/:) <$> possibilities (x:/:a)

instance
    ( Num a
    , Num b
    , Bounded b
    ) => Num (a:/:b)
instance
    ( Integral a
    , Integral b
    , Bounded b
    ) => Integral (a:/:b)
instance
    ( Bounded a
    , Bounded b
    ) => Bounded (a:/:b)

instance
    ( Integral x
    , Integral a
    , Possibilities x a
    , Possibilities (x:/:a) b
    , Total b
    ) => Total (a:/:b) where
    total (a:/:b) = preceeding + total b where
        preceeding = fromIntegral $ sum $ take (toInt a) lengths
        expanded = expand <$> possibilities 0
        expand a' = length $ littles (0:/:a')
        lengths
            | a > 0 = afters expanded
            | otherwise = befores $ right expanded
        littles = possibilities :: (x:/:a) -> Zipper b

instance
    ( Alter a c
    , Alter b d
    ) => Alter (a:/:b) (c:/:d) where
    plus = pmap2 plus plus
    minus = pmap2 minus minus
    clobber = pmap2 clobber clobber

-- | Utilities
pmap2 :: (a -> c -> x) -> (b -> d -> y) -> (a:/:b) -> (c:/:d) -> (x:/:y)
pmap2 f g (a:/:b) = pmap (f a) (g b)

pmap :: (a -> x) -> (b -> y) -> (a:/:b) -> (x:/:y)
pmap f g (a:/:b) = f a :/: g b

-- | TODO: Extract
toInt :: Integral a => a -> Int
toInt = fromIntegral . toInteger
