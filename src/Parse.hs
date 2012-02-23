{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
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

type Parser = GenParser Char ()
class Parse a where parse :: Parser a
instance Parse Int where parse = read <$> many1 digit
instance Parse Integer where parse = read <$> many1 digit
instance Parse Rational where parse = (%) <$> (parse <* slash) <*> parse
instance Parse a => Parse (Maybe a) where parse = optionMaybe parse

colin, dot, slash, whitespace :: Parser ()
colin = string ":" *> pure ()
dot = string "." *> pure ()
slash = string "/" *> pure ()
whitespace = many1 (oneOf " \t") *> pure ()

whited :: Parser a -> Parser a
whited x = w *> x <* w where w = many (oneOf " \t")

liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f
