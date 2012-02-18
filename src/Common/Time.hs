{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Time (Year, Month, Day, Time, Detail, parseTime) where
import Common.Parsec (Parse, whited, colin, dot)
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%), numerator, denominator)
import Text.ParserCombinators.Parsec hiding (parse)
import TypeLevel.List
import TypeLevel.Naturals
import Prelude hiding (break)

type Year = Integer
type Month = Integer
type Day = Integer
type Time n =
    ( Applicative (List n)
    , Listable (List n)
    , ParseTime (List n) Integer
    , ParseTime (List n) (Maybe Integer)
    , DottedList (List n) Integer
    , DottedList (List n) (Maybe Integer)
    , Natural n
    )
type Detail = Rational



class ParseTime v a where
    parseTime    :: Parse (v a)
    parseTime = try parseFull <|> parsePartial <?> "time"

    parseFull    :: Parse (v a)
    parsePartial :: Parse (v a)
    showTime     :: v a -> String

instance ParseTime L0 a where
    parseFull = pure Nil
    parsePartial = pure Nil
    showTime = const ""

instance TimeElem a => ParseTime L1 a where
    parseFull = pure <$> (parse <* colin)
    parsePartial = pure <$> (parse <* colin)
    showTime (a:._) = display a ++ ":"

instance ( TimeElem a
         , Natural n
         , DottedList (List n) a
         ) => ParseTime (List (Succ (Succ n))) a where
    parseFull = (parse <* colin) `consA` (parse `consA` fullDots)
    parsePartial = (parse <* colin) `consA` (parse `consA` someDots)
    showTime (a:.b:.v) = display a ++ ":" ++ intercalate "." ds
        where ds = map display $ b : relevantDots v



class DottedList v a where
    fullDots :: Parse (v a)
    someDots :: Parse (v a)
    relevantDots :: v a -> [a]
    dots :: v a -> [a]

instance DottedList L0 a where
    fullDots = pure Nil
    someDots = pure Nil
    relevantDots = const []
    dots = const []

instance ( TimeElem a
         , Natural n
         , DottedList (List n) a
         , Applicative (List n)
         ) => DottedList (List (Succ n)) a where
    fullDots = (dot *> parse) `consA` fullDots
    someDots =
        try ((dot *> parse) `consA` someDots)
        <|> pure (pure none)
    relevantDots v | all (== none) (dots v) = []
    relevantDots (a:.v) = a : relevantDots v
    dots (a:.v) = a : dots v



class Eq a => TimeElem a where
    parse :: Parse a
    display :: a -> String
    none :: a

instance TimeElem Integer where
    parse = read <$> many1 digit -- TODO: remove int/maybeInt
    display = show
    none = 0

instance TimeElem (Maybe Integer) where
    parse = optionMaybe $ read <$> many1 digit
    display = maybe "" show
    none = Nothing


-- | Utilities
consA x y = (:.) <$> x <*> y
