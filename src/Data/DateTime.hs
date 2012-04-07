{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DateTime
    ( DateTime(..)
    , (:::)(..)
    , (:/:)(..)
    , DateLike
    , TimeLike
    ) where
import Control.Applicative
import Data.Coded
import Data.DateTime.ConstPart ((:::)(..))
import Data.DateTime.VarPart ((:/:)(..))
import Data.Modable
import Data.Normalize
import Data.Ratio (numerator, denominator)
import Data.Zeroed
import Text.Parse
import Text.ParserCombinators.Parsec ((<?>))

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

data DateTime d t = DateTime
    { day   :: d
    , time  :: t
    , extra :: Rational
    } deriving (Eq, Ord)

instance (Show d, Show t) => Show (DateTime d t) where
    show (DateTime d t x) = show d ++ " " ++ show t ++ "." ++ frac where
        frac = show (numerator x) ++ "%" ++ show (denominator x)

instance (DateLike d, TimeLike t) => Normalize (DateTime d t) where
    isNormal (DateTime d t _) = isNormal d && isNormal t
    normalize (DateTime d t e) = (p, DateTime d' t' e') where
        excess = truncate e :: Integer
        e' = e - fromIntegral excess
        (o, t') = normalize (add excess t)
        (p, d') = normalize (add o d)
        add 0 a = a
        add n a = if n > 0 then add (n-1) (succ a) else add (n+1) (pred a)

instance (Parse d, Parse t, Enum t) => Parse (DateTime d t) where
    parse = fullDate <|> noExtra <|> datePart <?> "A DateTime" where
        fullDate = DateTime <$> (parse <* whitespace)
            <*> parse <*> (dot *> parse)
        noExtra = DateTime <$> (parse <* whitespace) <*> parse <*> pure 0
        datePart = DateTime <$> parse <*> pure (toEnum 0) <*> pure 0
