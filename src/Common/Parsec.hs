{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Common.Parsec
    ( Parser
    , Parseable(parse)
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
class Parseable a where parse :: Parser a
instance Parseable Integer where parse = read <$> many1 digit
instance Parseable Rational where parse = (%) <$> (parse <* slash) <*> parse
instance Parseable a => Parseable (Maybe a) where parse = optionMaybe parse

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
