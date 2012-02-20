{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Enumable where
import Data.List.Zipper
import TypeLevel.Naturals hiding (n)

class Enumable a where
    zipper :: a -> Zipper a

instance Enumable Integer where
    zipper _ = Zip [-1,-2..] [0..]

instance Natural n => Enumable (Succ n) where
    zipper _ = fromList $ init [minBound..maxBound]


-- TODO: Make a zipper that has functor, applicative, monad
concatZip :: (a -> Zipper b) -> Zipper a -> Zipper b
concatZip fn (Zip ls rs) = Zip (concatMap (toList . fn) ls) (concatMap (toList . fn) rs)

zMap :: (a -> b) -> Zipper a -> Zipper b
zMap fn (Zip ls rs) = Zip (map fn ls) (map fn rs)

zConcat :: Zipper (Zipper a) -> Zipper a
zConcat (Zip ls rs) = Zip (concatMap toList ls) (concatMap toList rs)

travel :: (Integral n) => n -> (a -> a) -> a -> a
travel 0 _ xs = xs
travel n f xs | n < 0 = error "Infinite travel loop"
              | otherwise = travel (n-1) f (f xs)
