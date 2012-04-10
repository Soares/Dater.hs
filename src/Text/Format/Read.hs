{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Read
    ( readFormat
    , readFormatIn
    , readSpec
    , readSpecIn
    , blockParser
    , naturalParser
    , signParser
    , signedParser
    , sizedParser
    , sizedNaturalParser
    , sizedSignedParser
    , stringParser
    , disjointParser
    , paddedStringParser
    , padded
    , Loader(..)
    , ParseBlock
    , ReadBlock(..)
    ) where
import Control.Applicative
import Data.Char
import Data.Locale
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

type ParseBlock = (Padding -> Parser Integer, Casing -> Parser Integer)

class Loader x f where
    create :: Map f Integer -> x
    loadable :: Maybe (Locale x) -> f -> [ParseBlock]

class ReadBlock a where
    number :: a -> Integer
    parseTextual :: Casing -> Parser a
    parseTextual = nonTextual
    parseNumerical :: Padding -> Parser a
    parseNumerical = nonNumerical

-- | The standard non-textual fail parser
nonTextual :: Casing -> Parser a
nonTextual = const $ fail "block can not be parsed textually"

-- | The standard non-numerical fail parser
nonNumerical :: Padding -> Parser a
nonNumerical = const $ fail "block can not be parsed numerically"

-- | Utility to make a ParseBlock from a ReadBlock
blockParser :: forall a. ReadBlock a => a -> ParseBlock
blockParser _ = (fmap number . num, fmap number . text) where
    num = parseNumerical :: Padding -> Parser a
    text = parseTextual :: Casing -> Parser a


-- | Generate a parser according to a format string, without localization.
-- | `f` is a witness and may be undefined
readFormat :: forall f x. (Format f, Loader x f)
    => f -> String
    -> Either ParseError (Parser x)
readFormat = readFormatIn Nothing


-- | Generate a parser according to a format string, with localization
-- | `f` is a witness and may be undefined
readFormatIn :: forall f x. (Format f, Loader x f)
    => Maybe (Locale x)
    -> f -> String
    -> Either ParseError (Parser x)
readFormatIn loc _ str = readSpecIn loc <$> parsed where
    parsed = loadFormat str :: Either ParseError (Spec f)


-- | Generate a parser according to a spec, without localization
readSpec :: forall f x. (Format f, Loader x f) => Spec f -> Parser x
readSpec = readSpecIn Nothing


-- | Generate a parser according to a spec, with localization
readSpecIn :: forall f x. (Format f, Loader x f)
    => Maybe (Locale x) -> Spec f
    -> Parser x
readSpecIn loc spec = create <$> dict where
    segments = mapM parseFrom spec
    dict = Map.fromList . lefts <$> segments
    parseFrom :: Either (Section f) String -> Parser (Either (f, Integer) ())
    parseFrom (Right str) = Right <$> void (string str)
    parseFrom (Left sec) = Left . (,) tgt <$> get sty block where
        Section tgt sty p c n = sec
        blocks = loadable loc tgt
        block = force (nonNumerical, nonTextual) n blocks
        get Number = flip fst p
        get Name = flip snd c

-- | Parse a padded natural number
naturalParser :: Padding -> Parser Integer
naturalParser (Exactly c w) = read <$> choice
    [ try (count x (char c) *> count (w-x) digit) | x <- [0..w-1] ]
naturalParser _ = read <$> many1 digit

-- | Parse a +, -, or ± and make a parser that can modify numbers accordingly
-- | (± has no effect and should be intended for 0 values)
signParser :: Num a => Parser (a -> a)
signParser = try (oneOf "±+" *> pure id) <|> (char '-' *> pure negate)

-- | Parse a padded integer that must have a sign in front.
-- | Padding with a digit will parse the sign first, padding
-- | with a non-digit will parse the sign right before the number.
-- | (ex. (Exactly '.' 7) → '...+100', (Exactly '0' 7) → +000100)
signedParser :: Padding -> Parser Integer
signedParser p@(Exactly c w)
    | isDigit c = signParser <*> naturalParser (decrease p)
    | otherwise = padded c w num where
        num 1 = fail "not enough space for sign and digits"
        num u = signParser <*> (read <$> count (u-1) digit)
signedParser p = signParser <*> naturalParser p

-- | Apply a preferred width to an integer parser
sizedParser :: (Padding -> Parser Integer) -> Int -> Padding -> Parser Integer
sizedParser num w (Yours c) = num $ Exactly c w
sizedParser num _ p = num p

-- | A preferred-width natural parser
sizedNaturalParser :: Int -> Padding -> Parser Integer
sizedNaturalParser = sizedParser naturalParser

-- | A preferred-width signed number parser
sizedSignedParser :: Int -> Padding -> Parser Integer
sizedSignedParser = sizedParser signedParser

-- | Parse a specific string padded to a certain width.
-- | Doesn't do any fancy handling of signs.
paddedStringParser :: String -> Padding -> Parser String
paddedStringParser str (Exactly c w) = string (replicate delta c) *> string str
    where delta = w - length str
paddedStringParser str _ = string str

-- | Given a padding character, a padding width, and a parser that knows
-- | how to parse only up to a given width, generate a parser of the same
-- | type that parses exactly `width` characters.
padded :: Char -> Int -> (Int -> Parser a) -> Parser a
padded c w p = choice [ try (count u (char c)) *> p (w-u) | u <- [0..w-1] ]

-- | Generates a parser for a string, in accordance with casing rules
stringParser :: Casing -> String -> Parser String
stringParser Normal = string . id
stringParser Upper = string . map toUpper
stringParser Lower = string . map toLower
stringParser Inverted = string . map invert
    where invert c = if isLower c then toUpper c else toLower c

disjointParser ::
    [Padding -> Parser Integer] ->
    [Casing -> Parser Integer] ->
    [ParseBlock]
disjointParser a b = zip (cycle a) (cycle b)

instance ReadBlock Integer where
    parseNumerical = naturalParser
    number = id

instance ReadBlock Int where
    parseNumerical = fmap fromInteger . naturalParser
    number = toInteger

lefts :: [Either a b] -> [a]
lefts (Right _ : xs) = lefts xs
lefts (Left a : xs) = a : lefts xs
lefts [] = []
