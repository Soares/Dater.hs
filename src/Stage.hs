{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Stage where
import Calendar
import Control.Applicative
import Data.Ratio hiding ((%))
import Data.Vec ( Vec, VecList, Fold, Head, Tail, Map, ZipWith, (:.)(..), toList, fromList )
import Data.Vec.Nat
import qualified Data.Vec as Vec
import Prelude hiding ((!!))
import Range hiding (mod)
import Utils

class Maybeify a a'
instance Maybeify () ()
instance Maybeify (a :. ()) (Maybe a :. ())
instance (Maybeify v v') => Maybeify (a :. v) (Maybe a :. v')

{-
instance (Maybeify v) => Maybeify (a :. v)

Maybeify () = ()
Maybeify (a :. v) = Maybe a :. Maybeify v

class MaybiedVec v v' | v -> v' where
    maybeify :: v -> v'

instance MaybiedVec v 

class Maybeifiable a k v where
    maybeify :: a k v -> a (Maybe k) v'
instance Maybeifiable (:.) a () where
    maybeify (a :. ()) = Just a :. ()
instance (Maybeifiable (:.) x y) => Maybeifiable (:.) a (x :. y) where
    maybeify (a :. v) = Just a :. maybeify v
-}
