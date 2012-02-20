{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevel.DatePart where
import Control.Applicative
import Control.Arrow
import Enumable
import Data.List.Zipper
import Format
import Parse
import Prelude hiding (break)

data a :/: b = a :/: b deriving (Eq, Ord)
infixr :/:

class (Parse a, Format a, Enumable x) => DatePart a x y | a -> x, a -> y where
    break :: x -> (a, y)
    build :: a -> y -> x
    possibilities :: x -> Zipper (a, y)
    possibilities = zMap break . zipper

instance (Show a, Show b) => Show (a :/: b) where
    show (a :/: b) = show a ++ "/" ++ show b

instance (Parse a, Parse b) => Parse (a :/: b) where
    parse = (:/:) <$> (parse <* slash) <*> parse

instance (Format a, Format b) => Format (a :/: b) where
    display f (a :/: b) = display f a ++ "/" ++ display (descend f) b

instance (DatePart a x y, DatePart b y z) => DatePart (a :/: b) x z where
    break x = let
        (a, y) = break x
        (b, z) = break y
        in (a :/: b, z)
    build (a :/: b) z = build a (build b z)
    possibilities = zConcat . zMap (uncurry comb) . bigs where
        comb a = zMap (first (a :/:)) . littles
        bigs = possibilities :: x -> Zipper (a, y)
        littles = possibilities :: y -> Zipper (b, z)
