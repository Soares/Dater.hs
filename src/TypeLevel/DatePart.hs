{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevel.DatePart where
import Control.Applicative
import Enumable
import Data.List.Zipper
import Format
import Parse
import Prelude hiding (break)

data a :/: b = a :/: b deriving (Eq, Ord)
infixr :/:

class (Parse a, Format a) => DatePart a x y where
    break :: x -> (a, y)
    build :: a -> y -> x

instance (Show a, Show b) => Show (a :/: b) where
    show (a :/: b) = show a ++ "/" ++ show b
instance (Parse a, Parse b) => Parse (a :/: b) where
    parse = (:/:) <$> (parse <* slash) <*> parse
instance (Format a, Format b) => Format (a :/: b) where
    display f (a :/: b) = display f a ++ "/" ++ display (descend f) b
instance (ContextList x x, ContextList y y, DatePart a x y, DatePart b y z) => ContextList x (a :/: b) where
    zipper = zConcat . zMap (uncurry comb) . bigs where
        bigs = zBreak :: x -> Zipper (a, y)
        littles = zBreak :: y -> Zipper (b, z)
        comb a y = zMap ((a :/:) . fst) (littles y)
instance (DatePart a x y, DatePart b y z) => DatePart (a :/: b) x z where
    break x = let
        (a, y) = break x :: (a, y)
        (b, z) = break y :: (b, z)
        in (a :/: b, z)
    build (a :/: b) z = let
        y = build b z :: y
        x = build a y :: x
        in x

zBreak :: forall a x y. (DatePart a x y, ContextList x x) => x -> Zipper (a, y)
zBreak = zMap break . (zipper :: x -> Zipper x)
