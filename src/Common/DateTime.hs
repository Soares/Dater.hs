{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.DateTime (DateTime, parseDateTime) where
import Common.Parsec
import Common.Time
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Text.ParserCombinators.Parsec
import TypeLevel.List

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



-- | Parsers
parseDateTime :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseDateTime era =
    try (parseYMDTX era)
    <|> try (parseYMDT era)
    <|> try (parseYMD era)
    <|> parseY era
    <?> "a DateTime"

parseYMDTX :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMDTX era = addTX <$> base <*> parseTX where
    base = parseYMD era :: Parse (DateTime n, a)

parseYMDT :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMDT era = addT <$> (addMD <$> base <*> parseMD) <*> parseTime where
    base = parseY era :: Parse (DateTime n, a)

parseYMD :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMD era = addMD <$> parseY era <*> parseMD

parseY :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseY era = (,) <$> fmap fromY int <*> whited era
    where fromY y = DateTime y 0 0 (pure 0) 0

parseMD :: Parse (Month, Day)
parseMD = (,) <$> (int <* slash) <*> int

parseTX :: Time n => Parse (List n Integer, Detail)
parseTX = (,) <$> parseTime <*> (dot *> rational)


-- | Helper builders
addMD :: forall n a. (DateTime n, a) -> (Month, Day) -> (DateTime n, a)
addMD (d, a) (m, y) = (d{month=m, day=y}, a)

addTX :: forall n a. (DateTime n, a) -> (List n Integer, Detail) -> (DateTime n, a)
addTX (d, a) (t, x) = (d{time=t, detail=x}, a)

addT :: forall n a. (DateTime n, a) -> List n Integer -> (DateTime n, a)
addT (d, a) t = (d{time=t}, a)



-- | Implementation-specific helpers
dateTimeList :: forall n. Listable (List n) => DateTime n -> ([Integer], Detail)
dateTimeList dt = (year dt : month dt : day dt : toList (time dt), detail dt)
