{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Delta where
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Data.Maybe
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
    , Natural n
    )

data Delta n
    = Delta
    { year   :: Maybe Year
    , month  :: Maybe Month
    , day    :: Maybe Day
    , time   :: List n (Maybe Integer)
    , detail :: Maybe Detail
    }

deltaList dt = (year dt : month dt : day dt : toList (time dt), detail dt)

instance Time n => Eq (Delta n) where (==) = (==) `on` deltaList
instance Time n => Ord (Delta n) where (<=) = (<=) `on` deltaList

left :: Maybe a -> Maybe a -> Maybe a
left Nothing Nothing = Nothing
left (Just x) _ = Just x
left _ y = y

plus :: Time n => Delta n -> Delta n -> Delta n
plus (Delta y1 m1 d1 t1 x1) (Delta y2 m2 d2 t2 x2) = Delta
    (left y1 y2)
    (left m1 m2)
    (left d1 d2)
    (left <$> t1 <*> t2)
    (left x1 x2)

parseDelta :: forall n. Time n => Parse (Delta n)
parseDelta =
    try (plus <$> parseSupDay <*> (whitespace *> parseSubDay))
    <|> try parseSupDay
    <|> try parseSubDay
    <?> "a datetime Delta"

parseSupDay :: forall n. Time n => Parse (Delta n)
parseSupDay =
    try (fromYMD <$> parseMaybeInt <*> (slash *> parseMaybeInt) <*> (slash *> parseMaybeInt))
    <|> (fromY <$> parseInt)
    <?> "yyyy/mm/dd" where
    fromYMD y m d = nowhen{year=y, month=m, day=d}
    fromY y = nowhen{year=Just y}
    notime = pure Nothing :: List n (Maybe Integer)
    nowhen = Delta Nothing Nothing Nothing notime Nothing

parseSubDay :: forall n. Time n => Parse (Delta n)
parseSubDay =
    try (fromTX <$> parseTime <*> parseDetail)
    <|> (fromT <$> parseTime)
    <?> "hh:mm.ss..." where
    fromTX t x = nowhen{time=t,detail=x}
    fromT t = nowhen{time=t}
    -- TODO
    notime = pure Nothing :: List n (Maybe Integer)
    nowhen = Delta Nothing Nothing Nothing notime Nothing

--TODO: Not dry (see parseX)
parseDetail =
    try (Just <$> ((%) <$> (parseInt <* slash) <*> parseInt))
    <|> pure Nothing


parseInt :: Intlike a => Parse a
parseInt = read <$> many1 digit

parseMaybeInt :: Intlike a => Parse (Maybe a)
parseMaybeInt = optionMaybe $ read <$> many1 digit

class Intlike a => ParseTime v a where
    parseTime :: Parse (v (Maybe a))
    parseTime = try parseFull <|> parsePartial <?> "a Time"
    parseFull :: Parse (v (Maybe a))
    parsePartial :: Parse (v (Maybe a))
    showTime :: v (Maybe a) -> String

instance Intlike a => ParseTime L0 a where
    parseFull = pure Nil
    parsePartial = pure Nil
    showTime = const ""

instance Intlike a => ParseTime L1 a where
    parseFull = pure <$> (parseMaybeInt <* colin)
    parsePartial = pure <$> (parseMaybeInt <* colin)
    showTime (a:._) = show a ++ ":"

instance ( Intlike a
         , Natural n
         , DottedList (List n) a
         ) => ParseTime (List (Succ (Succ n))) a where
    parseFull = (:.) <$> (parseMaybeInt <* colin) <*>
        ((:.) <$> parseMaybeInt <*> parseFullDots)
    parsePartial = (:.) <$> (parseMaybeInt <* colin) <*>
        ((:.) <$> parseMaybeInt <*> parsePartialDots)
    showTime (a:.b:.v) = showIfJust a ++ ":" ++
        intercalate "." (map showIfJust $ b : relevantDots v)

class Intlike a => DottedList v a where
    parseFullDots :: Parse (v (Maybe a))
    parsePartialDots :: Parse (v (Maybe a))
    relevantDots :: v (Maybe a) -> [Maybe a]
    dots :: v (Maybe a) -> [Maybe a]

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
    parseFullDots = (:.) <$> (dot *> parseMaybeInt) <*> parseFullDots
    parsePartialDots =
        try ((:.) <$> (dot *> parseMaybeInt) <*> parsePartialDots)
        <|> pure (pure Nothing)
    relevantDots (Nothing:.v) | all isNothing (dots v) = []
    relevantDots v = dots v
    dots (a:.v) = a : dots v

-- TODO: Exctract into module. (Not DRY)
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

--TODO
showIfJust Nothing = ""
showIfJust (Just a) = show a
