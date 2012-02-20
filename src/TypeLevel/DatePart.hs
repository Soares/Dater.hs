{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevel.DatePart where
import Arithmetic
import Control.Applicative
import Enumable
import Data.List.Zipper
import Format
import Parse
import TypeLevel.Naturals
import Prelude hiding (break)

data a :/: b = a :/: b deriving (Eq, Ord)

instance (ContextList x x, DatePart a x y) => ContextList x (a, y) where
    zipper x = zMap (break :: x -> (a, y)) (zipper x :: Zipper x)

class (Parse a, Format a) => DatePart a x y where
    break :: x -> (a, y)
    build :: (a, y) -> x

instance (Negable a, Negable b) => Negable (a :/: b) where
    neg (a :/: b) = neg a :/: neg b
instance (Show a, Show b) => Show (a :/: b) where
    show (a :/: b) = show a ++ "/" ++ show b
instance (Parse a, Parse b) => Parse (a :/: b) where
    parse = (:/:) <$> (parse <* slash) <*> parse
instance (Format a, Format b) => Format (a :/: b) where
    display f (a :/: b) = display f a ++ "/" ++ display (descend f) b
instance (DatePart a x y, DatePart b y z) => DatePart (a :/: b) x z where
    break x = let
        (a, y) = break x :: (a, y)
        (b, z) = break y :: (b, z)
        in (a :/: b, z)
    build (a :/: b, z) = let
        y = build (b, z) :: y
        x = build (a, y) :: x
        in x
instance (ContextList x (a, y), ContextList y (b, z), DatePart a x y, DatePart b y z) => ContextList x (a :/: b) where
    zipper = zConcat . zMap (uncurry comb) . bigs where
        bigs = zipper :: x -> Zipper (a, y)
        littles = zipper :: y -> Zipper (b, z)
        comb a y = zMap ((a :/:) . fst) (littles y)
