{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.DateTime.Date where
import Control.Applicative hiding ((<|>))
import Data.Coded
import Data.Normalize
import Data.Ratio
import Text.Parse
import Text.ParserCombinators.Parsec hiding (Parser, parse)

data Date d t = Date
    { day   :: d
    , time  :: t
    , extra :: Rational
    } deriving (Eq, Ord)

instance (Show d, Show t) => Show (Date d t) where
    show (Date d t x) = show d ++ " " ++ show t ++ "." ++ frac where
        frac = show (numerator x) ++ "%" ++ show (denominator x)

instance (Enum d, Coded d, Normalize d, Enum t, Coded t, Normalize t) => Normalize (Date d t) where
    isNormal (Date d t _) = isNormal d && isNormal t
    normalize (Date d t e) = (p, Date d' t' e') where
        excess = truncate e :: Integer
        e' = e - (fromIntegral excess)
        (o, t') = normalize (add excess t)
        (p, d') = normalize (add o d)
        add 0 a = a
        add n a = if n > 0 then add (n-1) (succ a) else add (n+1) (pred a)

instance (Parse d, Parse t, Enum t) => Parse (Date d t) where
    parse = fullDate <|> noExtra <|> datePart <?> "A DateTime" where
        fullDate = Date <$> (parse <* whitespace) <*> parse <*> (dot *> parse)
        noExtra = Date <$> (parse <* whitespace) <*> parse <*> pure 0
        datePart = Date <$> parse <*> pure (toEnum 0) <*> pure 0
