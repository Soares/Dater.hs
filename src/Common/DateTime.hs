{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.DateTime where
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Text.ParserCombinators.Parsec
import TypeLevel.List
import TypeLevel.Naturals
type Parse = GenParser Char ()

-- | An earthlike date consists of the following:
-- | An outer unit
type Year = Integer
-- | A primary unit, dependent upon the secondary unit
type Month = Integer
-- | A secondary unit
type Day = Integer
-- | Leftover detail
type Detail = Rational

type Intlike a = (Integral a, Show a, Read a)
type Time n =
    ( Applicative (List n)
    , Listable (List n)
    , ParseTime (List n) Integer
    , DottedList (List n) Integer
    , Eq (List n Integer)
    , Ord (List n Integer)
    , Natural n
    )

data DateTime n
    = DateTime
    { year   :: Year
    , month  :: Month
    , day    :: Day
    , time   :: List n Integer
    , detail :: Detail
    }

dateTimeList dt = (year dt : month dt : day dt : toList (time dt), detail dt)

instance Time n => Eq (DateTime n) where (==) = (==) `on` dateTimeList
instance Time n => Ord (DateTime n) where (<=) = (<=) `on` dateTimeList

parseDateTime :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseDateTime era =
    try (parseYMDTX era)
    <|> try (parseYMDT era)
    <|> try (parseYMD era)
    <|> parseY era
    <?> "a DateTime"

addMD (d, a) (m, y) = (d{month=m, day=y}, a)

parseYMDT :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMDT era = addT <$> (addMD <$> base <*> parseMD) <*> parseTime where
    base = parseY era :: Parse (DateTime n, a)
    addT (d, a) t = (d{time=t}, a)
parseYMDTX :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMDTX era = addTX <$> base <*> parseTX where
    base = parseYMD era :: Parse (DateTime n, a)
    addTX (d, a) (t, x) = (d{time=t, detail=x}, a)
parseYMD :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseYMD era = addMD <$> parseY era <*> parseMD
parseY :: forall n a. Time n => Parse a -> Parse (DateTime n, a)
parseY era = (,) <$> fmap fromY parseInt <*> whited era
    where fromY y = DateTime y 0 0 (pure 0) 0

parseMD :: Parse (Month, Day)
parseTX :: ParseTime v a => Parse (v a, Detail)
parseX :: Parse Detail
parseMD = (,) <$> (parseInt <* slash) <*> parseInt
parseTX = (,) <$> parseFull <*> parseX
parseX = (%) <$> (parseInt <* slash) <*> parseInt

parseInt :: Intlike a => Parse a
parseInt = read <$> many1 digit

class Intlike a => ParseTime v a where
    parseTime :: Parse (v a)
    parseTime = try parseFull <|> parsePartial <?> "a Time"
    parseFull :: Parse (v a)
    parsePartial :: Parse (v a)
    showTime :: v a -> String

instance Intlike a => ParseTime L0 a where
    parseFull = pure Nil
    parsePartial = pure Nil
    showTime = const ""

instance Intlike a => ParseTime L1 a where
    parseFull = pure <$> parseInt
    parsePartial = pure <$> parseInt
    showTime (a:._) = show a

instance ( Intlike a
         , Natural n
         , DottedList (List n) a
         ) => ParseTime (List (Succ (Succ n))) a where
    parseFull = (:.) <$> (parseInt <* colin) <*>
        ((:.) <$> parseInt <*> parseFullDots)
    parsePartial = (:.) <$> (parseInt <* colin) <*>
        ((:.) <$> parseInt <*> parsePartialDots)
    showTime (a:.b:.v) = show a ++ ":" ++
        intercalate "." (map show $ b : relevantDots v)

class Intlike a => DottedList v a where
    parseFullDots :: Parse (v a)
    parsePartialDots :: Parse (v a)
    relevantDots :: v a -> [a]
    dots :: v a -> [a]

instance Intlike a => DottedList L0 a where
    parseFullDots = pure Nil
    parsePartialDots = pure Nil
    relevantDots = const []
    dots = const []
instance ( Intlike a
         , Natural n
         , DottedList (List n) a
         , Applicative (List n)
         ) => DottedList (List (Succ n)) a where
    parseFullDots = (:.) <$> (dot *> parseInt) <*> parseFullDots
    parsePartialDots =
        try ((:.) <$> (dot *> parseInt) <*> parsePartialDots)
        <|> pure (pure 0)
    relevantDots (0:.v) | all (== 0) (dots v) = []
    relevantDots v = dots v
    dots (a:.v) = a : dots v

colin, dot, slash, whitespace :: Parse ()
colin = string ":" *> pure ()
dot = string "." *> pure ()
slash = string "/" *> pure ()
whitespace = many1 (oneOf " \t") *> pure ()

whited :: Parse a -> Parse a
whited x = whitespace *> x <* whitespace

liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f
