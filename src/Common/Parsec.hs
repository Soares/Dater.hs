module Common.Parsec where
import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.Ratio (Ratio, (%))
import Text.ParserCombinators.Parsec

type Parse = GenParser Char ()

int :: (Integral a, Read a) => Parse a
int = read <$> many1 digit

rational :: (Integral a, Read a) => Parse (Ratio a)
rational = (%) <$> (int <* slash) <*> int

colin, dot, slash, whitespace :: GenParser Char st ()
colin = string ":" *> pure ()
dot = string "." *> pure ()
slash = string "/" *> pure ()
whitespace = many1 (oneOf " \t") *> pure ()

whited :: GenParser Char st a -> GenParser Char st a
whited x = whitespace *> x <* whitespace

liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f
