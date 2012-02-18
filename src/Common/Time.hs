{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Time
    ( Year
    , Month
    , Day
    , Time
    , Detail
    , parseTime
    , splitUp
    , dotSep
    , multiplicands
    ) where
import Common.Parsec (Parser, Parseable(parse), colin, dot)
import Control.Applicative hiding ((<|>))
import Data.List (intercalate)
import Text.ParserCombinators.Parsec hiding (Parser, parse)
import TypeLevel.List
import TypeLevel.Naturals
import Prelude hiding (break)

-- | Separate a list by dots
class DotSep v where dotSep :: Show a => v a -> String

instance Natural n => DotSep (List n) where
    dotSep Nil = ""
    dotSep (x:.Nil) = show x
    dotSep (x:.xs) = show x ++ "." ++ dotSep xs


-- | Given a time that is ordeded large unit to small unit (24:.60:.60:.Nil),
-- | break it into the multiplicands that allow the Time to be combined
-- | into one number all in the smallest unit (3600:.60:.1:.Nil)
class (Reduce v a, Num a) => Multiplicands v a where
    multiplicands :: v a -> v a

instance (Num a) => Multiplicands (List n) a where
    multiplicands Nil = Nil
    multiplicands (_:.v) = reduce (*) 1 v :. multiplicands v


-- | Given an integral and a Time that is ordered small unit to
-- | large unit (60:.60:.24:.Nil), split an integral along the
-- | time units (3783 becomes (1:.2:.3:.Nil))
-- | (Note that 3783 = 3600 + 180 + 3)
class (Integral a) => SplitUp a v where
    splitUp :: a -> v a -> v a

instance (Integral a) => SplitUp a (List n) where
    splitUp _ Nil = Nil
    splitUp n (a:.v) = mod n a :. splitUp (div n a) v


type Year = Integer
type Month = Integer
type Day = Integer
type Time n =
    ( Applicative (List n)
    , Reduce (List n) Integer
    , Reverse (List n)
    , Multiplicands (List n) Integer
    , SplitUp Integer (List n)
    , FromList (List n)
    , ParseTime (List n) Integer
    , ParseTime (List n) (Maybe Integer)
    , DotSep (List n)
    , DottedList (List n) Integer
    , DottedList (List n) (Maybe Integer)
    , Natural n
    )
type Detail = Rational


class ParseTime v a where
    parseTime    :: Parser (v a)
    parseTime = try parseFull <|> parsePartial <?> "time"

    parseFull    :: Parser (v a)
    parsePartial :: Parser (v a)
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
    showTime _ = error "ParseTime used with unnatural list"



class DottedList v a where
    fullDots :: Parser (v a)
    someDots :: Parser (v a)
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



class (Parseable a, Eq a) => TimeElem a where
    display :: a -> String
    none :: a

instance TimeElem Integer where
    display = show
    none = 0

instance TimeElem (Maybe Integer) where
    display = maybe "" show
    none = Nothing


-- | Utilities
consA :: forall (f :: * -> *) a n. (Applicative f, Natural n) =>
    f a -> f (List n a) -> f (List (Succ n) a)
consA x y = (:.) <$> x <*> y
