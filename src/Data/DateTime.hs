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
    , DateLike
    , TimeLike
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

type DateTimeLike d t z =
    ( DateLike d
    , TimeLike t
    , ZoneLike z
    )

type DateLike d =
    ( Zeroed d
    , Enum d
    , Coded d
    , Normalize d
    , Modable d
    , Eq d
    , Ord d
    , Show d
    , Eq (Relative d)
    , Ord (Relative d)
    , Show (Relative d)
    )

type TimeLike t =
    ( Integral t
    , Bounded t
    , Coded t
    , Normalize t
    , Modable t
    , Show t
    , Eq (Relative t)
    , Ord (Relative t)
    , Show (Relative t)
    )

type ZoneLike z =
    ( Normalize z
    )

data DateTime d t z = DateTime
    { date     :: d
    , time     :: t
    , extra    :: Rational
    , zone     :: z
    } deriving (Eq, Ord)

instance (Show d, Show t, Show z) => Show (DateTime d t z) where
    show (DateTime d t x z) = printf "%s %s.%s %s" d' t' x' z' where
        d' = show d :: String
        t' = show t :: String
        x' = printf "%d/%d" (numerator x) (denominator x) :: String
        z' = show z :: String

instance DateTimeLike d t z => Normalize (DateTime d t z) where
    isNormal (DateTime d t x z) =
        isNormal z && x < 1 && isNormal t && isNormal d
    normalize (DateTime d t x z) = (p, DateTime d' t' x' z') where
        excess = truncate x :: Integer
        x' = x - fromIntegral excess
        (offset, z') = normalize z
        (o, t') = normalize (add (toInteger offset + excess) t)
        (p, d') = normalize (add o d)
        add 0 a = a
        add n a = if n > 0 then add (n-1) (succ a) else add (n+1) (pred a)

instance DateTimeLike d t z => WriteBlock (DateTime d t z) where
    numerical udt = show $ let
        dt = normal udt
        ymd = encode (date dt)
        hms = encode (time dt)
        size = toInteger (succ (maxBound - minBound) :: t)
        in (ymd*size)+hms
