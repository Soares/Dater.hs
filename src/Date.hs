{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Date where
import Control.Applicative hiding ((<|>))
import Data.Ratio
import Parse
import Format
import Text.ParserCombinators.Parsec hiding (Parser, parse)

data Date d t = Date
    { day   :: d
    , time  :: t
    , extra :: Rational
    }

instance (Show d, Show t) => Show (Date d t) where
    show (Date d t x) = show d ++ " " ++ show t ++ "." ++ frac where
        frac = show (numerator x) ++ "/" ++ show (denominator x)

fullDate :: (Parse d, Parse t) => Parser (Date d t)
fullDate = Date <$> (parse <* whitespace) <*> parse <*> (dot *> parse)
noExtra :: (Parse d, Parse t) => Parser (Date d t)
noExtra = Date <$> (parse <* whitespace) <*> parse <*> pure 0
datePart :: (Parse d, Parse t, Enum t) => Parser (Date d t)
datePart = Date <$> parse <*> pure (toEnum 0) <*> pure 0

instance (Parse d, Parse t, Enum t) => Parse (Date d t) where
    parse = fullDate <|> noExtra <|> datePart <?> "A DateTime"

instance (Format () d, Format d t) => Format () (Date d t) where
    display () (Date d t x) c = d' ++ t' ++ x' where
        d' = display () d c
        t' = display d t (descend $ invert $ c)
        x' = if isForMe c
                then case style c of
                    Number -> show x
                    Name -> show $ numerator x
                    Abbreviation i -> show $ floor $ x*10^i
                else ""
