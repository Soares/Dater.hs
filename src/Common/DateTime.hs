{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.DateTime (DateTime(..), showInEra, parseInEra) where
import Common.Parsec (Parser, parse, whited, slash, dot)
import Common.Time
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Text.ParserCombinators.Parsec hiding (Parser, parse)
import TypeLevel.List
import TypeLevel.Naturals
import Utils (showRatio)
import Text.Printf (printf)

data DateTime n
    = DateTime
    { year   :: Year
    , month  :: Month
    , day    :: Day
    , time   :: List n Integer
    , detail :: Detail
    }



instance Time n => Eq (DateTime n) where (==) = (==) `on` dateTimeList
instance Time n => Ord (DateTime n) where (<=) = (<=) `on` dateTimeList


-- | Displaying
showInEra :: Time n => String -> DateTime n -> String
showInEra e (DateTime y m d t x) = printf "%s %s/%s/%s%s" y e m d
    (timeTail (showTime t) (showRatio x)) where
    timeTail "" "0" = ""
    timeTail ts "0" = printf " %s" ts
    timeTail "" str = printf " %s" str
    timeTail ts str = printf " %s.%s" ts str



-- | Parsers
parseInEra :: forall n a. Time n => Parser a -> Parser (DateTime n, a)
parseInEra era =
    try (parseYMDTX era)
    <|> try (parseYMDT era)
    <|> try (parseYMD era)
    <|> parseY era
    <?> "a DateTime"

parseYMDTX :: forall n a. Time n => Parser a -> Parser (DateTime n, a)
parseYMDTX era = addTX <$> base <*> parseTX where
    base = parseYMD era :: Parser (DateTime n, a)

parseYMDT :: forall n a. Time n => Parser a -> Parser (DateTime n, a)
parseYMDT era = addT <$> (addMD <$> base <*> parseMD) <*> parseTime where
    base = parseY era :: Parser (DateTime n, a)

parseYMD :: forall n a. Time n => Parser a -> Parser (DateTime n, a)
parseYMD era = addMD <$> parseY era <*> parseMD

parseY :: forall n a. Time n => Parser a -> Parser (DateTime n, a)
parseY era = (,) <$> fmap fromY parse <*> whited era
    where fromY y = DateTime y 0 0 (pure 0) 0

parseMD :: Parser (Month, Day)
parseMD = (,) <$> (parse <* slash) <*> parse

parseTX :: Time n => Parser (List n Integer, Detail)
parseTX = (,) <$> parseTime <*> (dot *> parse)


-- | Helper builders
addMD :: (DateTime n, a) -> (Month, Day) -> (DateTime n, a)
addMD (d, a) (m, y) = (d{month=m, day=y}, a)

addTX :: (DateTime n, a) -> (List n Integer, Detail) -> (DateTime n, a)
addTX (d, a) (t, x) = (d{time=t, detail=x}, a)

addT :: (DateTime n, a) -> List n Integer -> (DateTime n, a)
addT (d, a) t = (d{time=t}, a)



-- | Implementation-specific helpers
dateTimeList :: forall n. Natural n => DateTime n -> ([Integer], Detail)
dateTimeList dt = (year dt : month dt : day dt : toList (time dt), detail dt)
