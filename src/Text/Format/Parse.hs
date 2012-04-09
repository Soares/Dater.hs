module Text.Format.Parse where
import Control.Applicative
import qualified Data.Map as Map
import Text.Format.Table
import Text.ParserCombinators.Parsec hiding ((<|>), many)
-- TODO: options need to be mabified or shortcuts won't work.

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
    = Padding Char Int
    | Case Casing
    | Alternate Int
    deriving (Eq, Read, Show)

opts :: Parser Options
opts = foldr reduce defaults <$> many opt where
    reduce (Padding c w) o = o{padding=(c, w)}
    reduce (Case c) o = o{casing=c}
    reduce (Alternate i) o = o{alternate=i}

opt :: Parser Option
opt =
    try (char '-' *> pure (Padding '0' 0))
    <|> try (Padding ' ' <$> padCount '_')
    <|> try (Padding '0' <$> padCount '0')
    <|> try (char '^' *> pure (Case Upper))
    <|> try (char '&' *> pure (Case Lower))
    <|> try (char '#' *> pure (Case Inverted))
    <|> (Alternate <$> altCount)
    <?> "a chunk option"

altCount :: Parser Int
altCount = either id id <$> countOf ':'

padCount :: Char -> Parser Int
padCount = (either id (\x->if x == 1 then 0 else x) <$>) . countOf

countOf :: Char -> Parser (Either Int Int)
countOf c =
    try (char c *> ((Left . read) <$> many1 digit))
    <|> (many1 (char c) >>= pure . Right . length)
