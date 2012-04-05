{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Parse
    ( Parser
    , Parse(parse)
    , colin
    , dot
    , slash
    , whitespace
    , whited
    , liftReadS
    ) where
import Control.Applicative hiding (many)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Text.ParserCombinators.Parsec hiding (Parser, parse)

-- | Shorthand for a parser with no state
type Parser = GenParser Char ()

-- | Things that can be parsed
class Parse a where parse :: Parser a

-- | Common characters used in dates
colin, dot, slash :: Parser ()
colin = string ":" *> pure ()
dot = string "." *> pure ()
slash = string "/" *> pure ()

-- | Make a parser 'floating'
-- | A floating parser discards surrounding whitespace
whited :: Parser a -> Parser a
whited x = w *> x <* w where w = many (oneOf " \t")

-- | Parse and discard at least one unit of whitespace
whitespace :: Parser ()
whitespace = many1 (oneOf " \t") *> pure ()

-- | Utility function to lift a 'read' into a parser
liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f

-- | Simple instances
instance Parse Int where parse = read <$> many1 digit
instance Parse Integer where parse = read <$> many1 digit
instance Parse Rational where parse = (%) <$> (parse <* slash) <*> parse
instance Parse a => Parse (Maybe a) where parse = optionMaybe parse
