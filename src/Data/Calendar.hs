{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Calendar
    ( Calendar(..)
    , (:/)(..)
    , (:\)(..)
    , CalendarLike
    , ZoneLike
    ) where
import Control.Applicative
import Data.Calendar.Composers
import Data.Modable
import Data.Normalize
import Data.Ratio (numerator, denominator)
import Text.Format.Write
import Text.Printf (printf)

type CalendarLike dt z =
    ( Integral dt
    , Real dt
    , Num dt
    , Ord dt
    , Eq dt
    , Normalize dt
    , Modable dt
    , Show dt
    , ZoneLike z
    )

type ZoneLike z =
    ( Normalize z
    )

data Calendar dt z = Calendar
    { dateTime :: dt
    , extra    :: Rational
    , zone     :: z
    } deriving (Eq, Ord)

instance (Show dt, Show z) => Show (Calendar dt z) where
    show (Calendar dt x z) = printf "%s.%s %s" (show dt) x' (show z) where
        x' = printf "%d/%d" (numerator x) (denominator x) :: String

instance CalendarLike dt z => Normalize (Calendar dt z) where
    isNormal (Calendar dt x z) =
        isNormal z && x < 1 && isNormal dt
    normalize (Calendar dt x z) = (o, Calendar dt' x' z') where
        excess = truncate x :: Int
        x' = x - fromIntegral excess
        (offset, z') = normalize z
        (o, dt') = normalize (dt + fromIntegral (offset + excess))

instance (Integral dt, Normalize dt, Normalize z) => WriteBlock (Calendar dt z) where
    numerical = pure . show . toInteger . normal . dateTime
