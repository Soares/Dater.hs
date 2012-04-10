{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Read
    ( InSection(In, InIn)
    , readFormat
    , readFormatIn
    , readSpec
    , readSpecIn
    , nameParser
    , naturalParser
    , signParser
    , signedParser
    , sizedParser
    , sizedNaturalParser
    , sizedSignedParser
    , Loader(..)
    , Loadable(..)
    , LoadableIn(..)
    , cased
    ) where
import Control.Applicative
import Data.Char
import Data.Locale
-- TODO? import Data.Coded
import Text.Format.Table
import Text.Format.Parse (loadFormat)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
    ( Parser
    , ParseError
    , char
    , digit
    , string
    , try
    , many1
    , count
    , choice
    , oneOf
    )

data InSection x
    = forall a. Loadable a => In a
    | forall b. LoadableIn b x => InIn b

readFormat :: forall f x. (Format f, Loader x f)
    => f -> String -> Either ParseError (Parser x)
readFormat = readFormatIn Nothing

readFormatIn :: forall f x. (Format f, Loader x f)
    => Maybe (Locale x)
    -> f -> String
    -> Either ParseError (Parser x)
readFormatIn loc _ str = readSpecIn loc <$> parsed where
    parsed = loadFormat str :: Either ParseError (Spec f)

readSpec :: forall f x. (Format f, Loader x f) => Spec f -> Parser x
readSpec = readSpecIn Nothing

readSpecIn :: forall f x. (Format f, Loader x f)
    => Maybe (Locale x) -> Spec f
    -> Parser x
readSpecIn loc spec = create <$> dict where
    segments = mapM parseFrom spec
    dict = Map.fromList . lefts <$> segments
    parseFrom :: Either (Section f) String -> Parser (Either (f, Integer) ())
    parseFrom (Right str) = Right <$> void (string str)
    parseFrom (Left sec) = Left . (,) tgt <$> get where
        Section tgt sty p c a = sec
        elm = loadable (undefined::x) tgt a
        get = parseSec loc sty a elm p c

parseSec :: forall x. Maybe (Locale x)
    -> Style -> Int -> InSection x
    -> Padding -> Casing
    -> Parser Integer
parseSec loc sty n sec p c = case (sty, sec) of
    (Name, In x) -> parseName x n c
    (Name, InIn x) -> parseNameIn loc x n c
    (Number, In x) -> parseNumber x n p
    (Number, InIn x) -> parseNumberIn loc x n p

nameParser :: [(Integer, String)] -> Casing -> Parser Integer
nameParser ns c = choice $ map tupleParser ns
    where tupleParser (i, str) = cased c str *> pure i

naturalParser :: Padding -> Parser Integer
naturalParser (Exactly c w) = read <$> choice
    [ try (count x (char c) *> count (w-x) digit) | x <- [0..w-1] ]
naturalParser _ = read <$> many1 digit

signParser :: Parser (Integer -> Integer)
signParser = try (oneOf "±+" *> pure id) <|> (char '-' *> pure negate)

signedParser :: Padding -> Parser Integer
signedParser p@(Exactly c w)
    | isDigit c = signParser <*> naturalParser (decrease p)
    | otherwise = padded c w num where
        num 1 = fail "not enough space for sign and digits"
        num u = signParser <*> (read <$> count (u-1) digit)
signedParser p = signParser <*> naturalParser p

sizedParser :: (Padding -> Parser Integer) -> Int -> Padding -> Parser Integer
sizedParser num w (Yours c) = num $ Exactly c w
sizedParser num _ p = num p

sizedNaturalParser :: Int -> Padding -> Parser Integer
sizedNaturalParser = sizedParser naturalParser

sizedSignedParser :: Int -> Padding -> Parser Integer
sizedSignedParser = sizedParser signedParser

padded :: Char -> Int -> (Int -> Parser a) -> Parser a
padded c w p = choice [ try (count u (char c)) *> p (w-u) | u <- [0..w-1] ]

lefts :: [Either a b] -> [a]
lefts (Right _ : xs) = lefts xs
lefts (Left a : xs) = a : lefts xs
lefts [] = []

cased :: Casing -> String -> Parser String
cased Normal = string . id
cased Upper = string . map toUpper
cased Lower = string . map toLower
cased Inverted = string . map invert
    where invert c = if isLower c then toUpper c else toLower c

class Loader x f where
    create :: Map f Integer -> x
    loadable :: x -> f -> Int -> InSection x

class Loadable x where
    parseName :: x -> Int -> Casing -> Parser Integer
    parseName = const . const $ nameParser []
    parseNumber :: x -> Int -> Padding -> Parser Integer
    parseNumber = const . const $ naturalParser

class LoadableIn a x where
    parseNameIn :: Maybe (Locale x) -> a -> Int -> Casing -> Parser Integer
    parseNameIn = const . const . const $ nameParser []
    parseNumberIn :: Maybe (Locale x) -> a -> Int -> Padding -> Parser Integer
    parseNumberIn = const . const . const $ naturalParser

instance Loadable [(Integer, String)] where parseName x _ = nameParser x
instance Loadable Integer
instance Loadable Int
