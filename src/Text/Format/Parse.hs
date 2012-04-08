module Text.Format.Parser where
import Control.Applicative
import qualified Data.Map as Map
import Text.Format.Table
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as P

parseFormat :: Format f => Parser [Either (Section f) String]
parseFormat = flatten <$> many1 section

shortcut :: Format f => String -> [Either (Options, Directive f) String]
shortcut str = let Right res = P.parse (many1 section) "shortcut" str in res

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
opts = foldr reduce defaults <$> many1 opt where
    reduce (Padding c w) o = o{padding=(c, w)}
    reduce (Case c) o = o{casing=c}
    reduce (Alternate i) o = o{alternate=i}

opt :: Parser Option
opt =
    try (char '-' *> pure (Padding '0' 0))
    <|> try (Padding ' ' <$> countOf '_')
    <|> try (Padding '0' <$> countOf '0')
    <|> try (char '^' *> pure (Case Upper))
    <|> try (char '&' *> pure (Case Lower))
    <|> try (char '#' *> pure (Case Inverted))
    <|> (Alternate <$> countOf ':')
    <?> "a chunk option"

countOf :: Char -> Parser Int
countOf c =
    try (many1 (char c) >>= pure . length)
    <|> (char c *> (read <$> many1 digit))
