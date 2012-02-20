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
import Data.List.Zipper
import Format
import Parse
import TypeLevel.Naturals
import Prelude hiding (break)

data a :/: b = a :/: b deriving (Eq, Ord)

class ContextList x a where
    zipper :: x -> Zipper a

-- instance ContextList Integer Integer where zipper _ = Zip [-1,-2..] [0..]
--
-- instance ContextList (Integer :/: Year) Month where
--  zipper _ = zZipper :: Zipper $(zMod 12)
--
-- instance ContextList (Integer :/: Year :/: Month) Day where
--  zipper (_ :/: y :/: m) | zZipper :: Zipper $(zMod x) where
--      x | y == Common && m == February = 28
--        | y == Leap && m == February = 29
--        | m `elem` [September, April, June, November] = 30
--        | otherwise = 31

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
{-
instance ( ContextList x a
         , ContextList a b
         ) => ContextList x b where
    zipper x = concatZip (zipper :: a -> Zipper b) (zipper x :: Zipper a)
    -}


{-
instance (FiniteEnum a x, FiniteEnum b (x :/: a), Bounded b) => FiniteEnum (a :/: b) x where
    next x = advance x next
    prev x = advance x prev
instance (Num x, Num y, Ord x, Ord y, Arithmetic a x c, Arithmetic b y c, DatePart b (x :/: a), Bounded b) => Arithmetic (a :/: b) (x :/: y) c where
    plus = undefined
    lift = thread (const lift) (const lift) (undefined :/: undefined)


-- Advance the outermost DatePart
{-
jump :: (Applicative f, DatePart b a) =>
    (a -> f a) -> (a :/: b) -> f (a :/: b)
    -}
jump fn (a :/: b) = (:/:) <$> a' <*> b' where
    a' = fn a
    i = build a (b, 0)
    b' = fst . flip break i <$> a'

-- Advance the innermost DatePart
advance :: (Bounded b, FiniteEnum b) =>
    (b -> Either NotInRange b) -> (a :/: b) -> Either NotInRange (a :/: b)
advance x fn (a :/: b) = emap (edge a) (Right . (a :/:)) (fn b) where

op (a :/: b) (x :/: y)
    | x == 0 && y == 0 = Right (a :/: b)
    | y > 0 = case next b of
                Right b' -> op (a :/: b) (0 :/: (y-1))
    | y < 0 = case next b of
                Right b' -> op (a :/: b) (0 :/: (y-1))
    | x > 0 = case jump next (a :/: b) of
                Right (a' :/: b') -> op (a' :/: b') ((x-1) :/: y)
    | otherwise = case jump prev (a :/: b) of
                Right (a' :/: b') -> op (a' :/: b') ((x+1) :/: y)

thread :: Num x =>
    (a -> x -> Either NotInRange a) -> (b -> y -> Either NotInRange b) ->
    (a :/: b) -> (x :/: y) -> Either NotInRange (a :/: b)
thread fx fy (a :/: b) (x :/: y) = case fx a x of
    Right x' -> case fy b y of
        Right y' -> Right $ x' :/: y'
        Left Overflow -> thread fx fy (a :/: b) ((x - 1) :/: y)
        Left Underflow -> thread fx fy (a :/: b) ((x + 1) :/: y)
    Left err -> Left err

emap f _ (Left a) = f a
emap _ g (Right b) = g b

edge a Overflow = over a
edge a Underflow = under a

-}
concatZip :: (a -> Zipper b) -> Zipper a -> Zipper b
concatZip fn (Zip ls rs) = Zip (concatMap (toList . fn) ls) (concatMap (toList . fn) rs)

zZipper :: (Bounded n, Enum n, PosInt n) => Zipper n
zZipper = fromList members

zMap fn (Zip ls rs) = Zip (map fn ls) (map fn rs)
zConcat (Zip ls rs) = Zip (concatMap toList ls) (concatMap toList rs)

members :: (Bounded e, Enum e) => [e]
members = [minBound..maxBound]

travel 0 _ xs = xs
travel n f xs | n < 0 = error "Infinite travel loop"
              | otherwise = travel (n-1) f (f xs)
