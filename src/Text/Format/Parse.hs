module Text.Format.Parse where
import Control.Applicative
import qualified Data.Map as Map
import Text.Format.Table
import Text.ParserCombinators.Parsec hiding ((<|>), many)

loadFormat :: Format f
    => String -> Either ParseError [Either (Section f) String]
loadFormat = parse parseFormat "format"

parseFormat :: Format f => Parser [Either (Section f) String]
parseFormat = flatten <$> many1 section

shortcut :: Format f => String -> Directive f
shortcut str = case parse parseFormat "shortcut" str of
    Right res -> Shortcut res
    Left err -> error $ show err

section :: Format f => Parser (Either (Options, Directive f) String)
section =
    try (Right <$> (char '%' *> escape))
    <|> try (Left <$> (char '%' *> chunk))
    <|> (Right <$> many1 (noneOf "%"))

escape :: Parser String
escape =
    try (char '%' *> pure "%")
    <|> try (char 't' *> pure "\t")
    <|> try (char 'n' *> pure "\n")
    <?> "an escape character [%nt]"

chunk :: Format f => Parser (Options, Directive f)
chunk = (,) <$> opts <*> directive

directive :: Format f => Parser (Directive f)
directive = letter >>= \c ->
    let die = fail $ "unknown chunk directive: " ++ [c]
    in maybe die pure $ Map.lookup c table

data Option
    = P Char Int
    | C Casing
    | A Int
    deriving (Eq, Read, Show)

opts :: Parser Options
opts = foldr reduce defaults <$> many opt where
    reduce (P _ 0) o = o{pad=Just None}
    reduce (P c 1) o = o{pad=Just (Yours c)}
    reduce (P c w) o = o{pad=Just (Exactly c w)}
    reduce (C c) o = o{txt=Just c}
    reduce (A i) o = o{alt=Just i}

opt :: Parser Option
opt =
    try (char '-' *> pure (P undefined 0))
    <|> try (P ' ' <$> padCount '_')
    <|> try (P '0' <$> padCount '0')
    <|> try (char '^' *> pure (C Upper))
    <|> try (char '&' *> pure (C Lower))
    <|> try (char '#' *> pure (C Inverted))
    <|> (A <$> altCount)
    <?> "a chunk option"

altCount :: Parser Int
altCount = either id id <$> countOf ':'

padCount :: Char -> Parser Int
padCount = (either id (\x->if x == 1 then 0 else x) <$>) . countOf

countOf :: Char -> Parser (Either Int Int)
countOf c =
    try (char c *> ((Left . read) <$> many1 digit))
    <|> (many1 (char c) >>= pure . Right . length)
