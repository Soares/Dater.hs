{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Read where
import Control.Applicative
import Control.Arrow
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
        Section tgt sty opt = sec
        Options pad cas alt = opt
        elm = loadable (undefined::x) tgt alt
        int = parseSec sty elm cas pad

parseSec :: Style -> InSection -> Casing -> (Char, Int) -> Parser Integer
parseSec Name sec c _ = nameParser (names sec) c
parseSec (Abbreviation i) sec c _ = nameParser (abbreviations i sec) c
parseSec Number sec _ p = numberParser (signed sec) (digits sec) p

nameParser :: [(Integer, String)] -> Casing -> Parser Integer
nameParser ns Upper = nameParser (map (second upper) ns) Normal
    where upper = map toUpper
nameParser ns Lower = nameParser (map (second lower) ns) Normal
    where lower = map toLower
nameParser ns Inverted = nameParser (map (second invert) ns) Normal
    where invert = map (\c -> if isLower c then toUpper c else toLower c)
nameParser ns Normal = choice $ map tupleParser ns
    where tupleParser (i, str) = string str *> pure i

numberParser :: Bool -> Int -> (Char, Int) -> Parser Integer
numberParser True d p =
    try (char '+' *> numberParser False d p)
    <|> try (char '-' *> (negate <$> numberParser False d p))
    <|> numberParser False d p
numberParser False _ (_, 0) = read <$> many1 digit
numberParser False d (c, 1) = read <$> paddedDigits c d
numberParser False _ (c, w) = read <$> paddedDigits c w

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

class Loader x f where
    create :: Map f Integer -> x
    loadable :: x -> f -> Int -> InSection

class Loadable x where
    names :: x -> [(Integer, String)]
    names = const []
    abbreviations :: Int -> x -> [(Integer, String)]
    abbreviations i = map (second $ take i) . names
    digits :: x -> Int
    digits = const 0
    signed :: x -> Bool
    signed = const False

instance Loadable InSection where
    names (In s) = names s
    abbreviations i (In s) = abbreviations i s
    digits (In s) = digits s
    signed (In s) = signed s

instance Loadable [(Integer, String)] where names = id
instance Loadable Integer
instance Loadable Int
