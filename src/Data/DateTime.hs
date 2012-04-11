{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DateTime
    ( DateTime(..)
    , (:::)(..)
    , (:/:)(..)
    , DateTimeLike
    , ZoneLike
    ) where
import Data.Coded
import Data.DateTime.ConstPart ((:::)(..))
import Data.DateTime.VarPart ((:/:)(..))
import Data.Modable
import Data.Normalize
import Data.Ratio (numerator, denominator)
import Data.Zeroed
import Text.Format.Write
import Text.Printf (printf)

type DateTimeLike dt z =
    ( Zeroed dt
    , Integral dt
    , Normalize dt
    , Modable dt
    , Coded dt
    , Show dt
    , Eq (Relative dt)
    , Ord (Relative dt)
    , Show (Relative dt)
    , ZoneLike z
    )

type ZoneLike z =
    ( Normalize z
    )

data DateTime dt z = DateTime
    { dateTime :: dt
    , extra    :: Rational
    , zone     :: z
    } deriving (Eq, Ord)

instance (Show dt, Show z) => Show (DateTime dt z) where
    show (DateTime dt x z) = printf "%s.%s %s" (show dt) x' (show z) where
        x' = printf "%d/%d" (numerator x) (denominator x) :: String

instance DateTimeLike dt z => Normalize (DateTime dt z) where
    isNormal (DateTime dt x z) =
        isNormal z && x < 1 && isNormal dt
    normalize (DateTime dt x z) = (o, DateTime dt' x' z') where
        excess = truncate x :: Int
        x' = x - fromIntegral excess
        (offset, z') = normalize z
        (o, dt') = normalize (dt + fromIntegral (offset + excess))

instance (Integral dt, Normalize dt, Normalize z) => WriteBlock (DateTime dt z) where
    numerical = show . toInteger . normal . dateTime
