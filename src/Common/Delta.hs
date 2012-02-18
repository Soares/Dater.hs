{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Delta (Delta(..)) where
import Common.Time
import Common.Parsec (Parser, Parseable(parse), whitespace, slash, dot)
import Control.Applicative hiding ((<|>))
import Data.Function (on)
import Data.List (intercalate)
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (Parser, parse)
import TypeLevel.List
import Text.Printf (printf)
import Prelude hiding (break)
import Utils (showRatio)

data Delta n
    = Delta
    { year   :: Maybe Year
    , month  :: Maybe Month
    , day    :: Maybe Day
    , time   :: List n (Maybe Integer)
    , detail :: Maybe Detail
    }

instance Time n => Parseable (Delta n) where
    parse =
        try (combine left <$> parseSupDay <*> (whitespace *> parseSubDay))
        <|> try parseSupDay
        <|> try parseSubDay
        <?> "a datetime Delta"

instance Time n => Show (Delta n) where
    show (Delta y m d Nil x) = dropDots $ printf "%s/%s/%s.%s"
        (mShow y) (mShow m) (mShow d) (maybe "" showRatio x)
    show (Delta y m d (t:.ts) x) = dropDots $ printf "%s/%s/%s %s:%s.%s"
        (mShow y) (mShow m) (mShow d) (mShow t)
        (intercalate "." $ map mShow $ toList ts)
        (maybe "" showRatio x) where


instance Time n => Num (Delta n) where
    (+) = combine $ liftA2 (+)
    (-) = combine $ liftA2 (-)
    (*) = combine $ liftA2 (*)
    abs = alter $ liftA abs
    signum = alter $ liftA signum
    fromInteger n = Delta (Just n) Nothing Nothing (pure Nothing) Nothing
-- TODO: Instance of whatever gives us clobber

instance Time n => Eq (Delta n) where (==) = (==) `on` deltaList
instance Time n => Ord (Delta n) where (<=) = (<=) `on` deltaList

parseSupDay :: forall n. Time n => Parser (Delta n)
parseSupDay =
    try (fromYMD <$> parse <*> (slash *> parse) <*> (slash *> parse))
    <|> (fromInteger <$> parse)
    <?> "yyyy/mm/dd" where
    fromYMD y m d = nowhen{year=y, month=m, day=d}
    notime = pure Nothing :: List n (Maybe Integer)
    nowhen = Delta Nothing Nothing Nothing notime Nothing

parseSubDay :: forall n. Time n => Parser (Delta n)
parseSubDay =
    try (fromTX <$> parseTime <*> (dot *> parse))
    <|> (fromT <$> parseTime)
    <?> "hh:mm.ss..." where
    fromTX t x = nowhen{time=t,detail=x}
    fromT t = nowhen{time=t}
    notime = pure Nothing :: List n (Maybe Integer)
    nowhen = Delta Nothing Nothing Nothing notime Nothing


-- | Manipulation helpers
left :: Maybe a -> Maybe a -> Maybe a
left Nothing Nothing = Nothing
left (Just x) _ = Just x
left _ y = y


-- | Relatively dangerous helper functions
deltaList :: Time n => Delta n -> [Maybe Rational]
deltaList d = xs ++ [x] where (x:xs) = break d

break :: Time n => Delta n -> [Maybe Rational]
break (Delta y m d ts x) = x : (toRational <$> y) : (toRational <$> m) : (toRational <$> d) : (fmap toRational <$> toList ts)

build :: Time n => [Maybe Rational] -> Delta n
build (x:y:m:d:ts) = Delta (round <$> y) (round <$> m) (round <$> d) (fmap round <$> fromList ts) x
build _ = error "Build used on a list that did not come from break"

alter :: Time n => (Maybe Rational -> Maybe Rational) -> Delta n -> Delta n
alter f = build . fmap f . break

combine :: Time n => (Maybe Rational -> Maybe Rational -> Maybe Rational) -> Delta n -> Delta n -> Delta n
combine f x y = build (f <$> break x <*> break y)

-- | Show helpers

mShow :: Show a => Maybe a -> String
mShow = maybe "" show

dropDots :: String -> String
dropDots = Prelude.reverse . dropWhile (== '.') . Prelude.reverse
