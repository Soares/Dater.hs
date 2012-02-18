-- TODO: Prune these
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.DateTime where
import Control.Applicative hiding ((<|>))
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Text.ParserCombinators.Parsec
import TypeLevel.List
import TypeLevel.Naturals
--TODO: Put reader/shower in data Common

type Year = Integer
-- | A primary unit, dependent upon the secondary unit
type Month = Integer
-- | A secondary unit
type Day = Integer
-- | (Year, Month, and Day can be abbreviated YMD)
type YMD = (Year, Month, Day)
-- | Leftover detail
type Detail = Rational

type Intlike a = (Integral a, Show a, Read a)
type NList v i =
    ( Applicative v
    , ParseTime v i
    , DottedList v i
    , Eq (v i)
    , Ord (v i)
    , Num (v i)
    , Num i
    )

data DateTime v
    = DateTime
    { year   :: Year
    , month  :: Month
    , day    :: Day
    , time   :: NList v Integer => v Integer
    , detail :: Detail
    }
-- TODO: Eq, Ord instances

parseDateTime :: NList v Integer =>
    GenParser Char st a ->
    GenParser Char st (DateTime v, a)
parseDateTime era =
    try (parseYMDTX era)
    <|> try (parseYMDT era)
    <|> try (parseYMD era)
    <|> parseY era
    <?> "a DateTime"

fromY y = DateTime y 0 0 (pure 0) 0
addMD (d, a) (m, y) = (d{month=m, day=y}, a)
addTX (d, a) (t, x) = (d{time=t, detail=x}, a)
addT (d, a) t = (d{time=t}, a)

parseYMDT era = addT <$> (addMD <$> parseY era <*> parseMD) <*> parseTime
parseYMDTX era = addTX <$> parseYMD era <*> parseTX
parseYMD era = addMD <$> parseY era <*> parseMD
parseY era = (,) <$> fmap fromY parseInt <*> whited era

parseMD :: GenParser Char st (Month, Day)
parseTX :: ParseTime v a => GenParser Char st (v a, Detail)
parseX :: GenParser Char st Detail
parseMD = (,) <$> (parseInt <* slash) <*> parseInt
parseTX = (,) <$> parseFull <*> parseX
parseX = (%) <$> (parseInt <* slash) <*> parseInt

parseInt :: Intlike a => GenParser Char st a
parseInt = read <$> many1 digit


class Intlike a => ParseTime v a where
    parseTime :: GenParser Char st (v a)
    parseTime = try parseFull <|> parsePartial <?> "a Time"
    parseFull :: GenParser Char st (v a)
    parsePartial :: GenParser Char st (v a)
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
         , DottedList (List n) a
         ) => ParseTime (List (Succ (Succ n))) a where
    parseFull = (:.) <$> (parseInt <* colin) <*>
        ((:.) <$> parseInt <*> parseFullDots)
    parsePartial = (:.) <$> (parseInt <* colin) <*>
        ((:.) <$> parseInt <*> parsePartialDots)
    showTime (a:.b:.v) = show a ++ ":" ++
        intercalate "." (map show $ b : relevantDots v)


class Intlike a => DottedList v a where
    parseFullDots :: GenParser Char st (v a)
    parsePartialDots :: GenParser Char st (v a)
    relevantDots :: v a -> [a]
    dots :: v a -> [a]

instance Intlike a => DottedList L0 a where
    parseFullDots = pure Nil
    parsePartialDots = pure Nil
    relevantDots = const []
    dots = const []
instance ( Intlike a
         , DottedList (List n) a
         , Applicative (List n)
         ) => DottedList (List (Succ n)) a where
    parseFullDots = (:.) <$> (dot *> parseInt) <*> parseFullDots
    parsePartialDots =
        try ((:.) <$> (dot *> parseInt) <*> parsePartialDots)
        <|> pure (pure 0)
    relevantDots (0:.v) | all (== 0) (relevantDots v) = []
    relevantDots v = dots v
    dots (a:.v) = a : dots v

colin, dot, slash, whitespace :: GenParser Char st ()
colin = string ":" *> pure ()
dot = string "." *> pure ()
slash = string "/" *> pure ()
whitespace = many1 (oneOf " \t") *> pure ()

whited :: GenParser Char st a -> GenParser Char st a
whited x = whitespace *> x <* whitespace

liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f
