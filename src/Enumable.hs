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
