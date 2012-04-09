{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Read where
import Control.Applicative
import Data.Char
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

data InSection = forall x. Loadable x => In x

readFormat :: forall f x. (Format f, Loader x f)
    => f -> String -> Either ParseError (Parser x)
readFormat _ str = readSections <$> parsed where
    parsed = loadFormat str :: Either ParseError (Spec f)

readSections :: forall f x. (Format f, Loader x f) => Spec f -> Parser x
readSections spec = create <$> dict where
    segments = mapM parseFrom spec
    dict = Map.fromList . lefts <$> segments

    parseFrom :: Either (Section f) String -> Parser (Either (f, Integer) ())
    parseFrom (Right str) = Right <$> void (string str)
    parseFrom (Left sec) = Left . (,) tgt <$> int where
        Section tgt sty p c a = sec
        elm = loadable (undefined::x) tgt a
        int = parseSec sty a elm p c

parseSec :: Style -> Int -> InSection -> Padding -> Casing -> Parser Integer
parseSec Name n sec _ c = parseName sec n c
parseSec Number n sec p _ = parseNumber sec n p

nameParser :: [(Integer, String)] -> Casing -> Parser Integer
nameParser ns c = choice $ map tupleParser ns
    where tupleParser (i, str) = cased c str *> pure i

naturalParser :: Padding -> Parser Integer
naturalParser (Exactly c w) = read <$> choice
    [ try (count x (char c) *> count (w-x) digit) | x <- [0..w-1] ]
naturalParser _ = read <$> many1 digit

sign :: Parser (Integer -> Integer)
sign =
    try (oneOf "±+" *> pure id)
    <|> (char '-' *> pure negate)

signedParser :: Padding -> Parser Integer
signedParser p@(Exactly c w)
    | isDigit c = sign <*> naturalParser (decrease p)
    | otherwise = choice
        [ try (count x (char c) *> (sign <*> (read <$> count (w-x-1) digit)))
        | x <- [0..w-2] ]
signedParser p = sign <*> naturalParser p

sizedParser :: (Padding -> Parser Integer) -> Int -> Padding -> Parser Integer
sizedParser num w (Yours c) = num $ Exactly c w
sizedParser num _ p = num p

sizedNaturalParser :: Int -> Padding -> Parser Integer
sizedNaturalParser = sizedParser naturalParser

sizedSignedParser :: Int -> Padding -> Parser Integer
sizedSignedParser = sizedParser signedParser

paddedDigits :: Char -> Int -> Parser String
paddedDigits c w
    | isDigit c = count w digit
    | otherwise = choice
        [ try (count x (char c) *> count (w-x) digit)
        | x <- [0..w-1] ]

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
    loadable :: x -> f -> Int -> InSection

-- TODO: Use encode/decode instead of sentinels
class Loadable x where
    parseName :: x -> Int -> Casing -> Parser Integer
    parseName = const $ const $ nameParser []
    parseNumber :: x -> Int -> Padding -> Parser Integer
    parseNumber = const $ const $ naturalParser

instance Loadable InSection where
    parseName (In s) = parseName s
    parseNumber (In s) = parseNumber s

instance Loadable [(Integer, String)] where parseName x _ = nameParser x
instance Loadable Integer
instance Loadable Int
