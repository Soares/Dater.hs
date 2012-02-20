{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Enumable where
import Data.List.Zipper
import TypeLevel.Naturals

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

instance Natural n => ContextList (Succ n) (Succ n) where
    zipper x = zZipper


-- TODO: Make a zipper that has applicative, mappend, functor, etc.
concatZip :: (a -> Zipper b) -> Zipper a -> Zipper b
concatZip fn (Zip ls rs) = Zip (concatMap (toList . fn) ls) (concatMap (toList . fn) rs)

zZipper :: (Bounded n, Enum n, PosInt n) => Zipper n
zZipper = fromList members

zMap fn (Zip ls rs) = Zip (map fn ls) (map fn rs)
zConcat (Zip ls rs) = Zip (concatMap toList ls) (concatMap toList rs)

members :: (Bounded e, Enum e) => [e]
members = init [minBound..maxBound]

travel 0 _ xs = xs
travel n f xs | n < 0 = error "Infinite travel loop"
              | otherwise = travel (n-1) f (f xs)
