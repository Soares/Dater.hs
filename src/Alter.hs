{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Alter where
import Control.Applicative
import Data.Maybe

class Alter a b where
    plus :: a -> b -> a
    minus :: a -> b -> a
    clobber :: a -> b -> a

instance Num a => Alter a (Maybe a) where
    plus = maybe <*> (+)
    minus = maybe <*> (-)
    clobber = fromMaybe
