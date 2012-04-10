module Text.Format.Block where

class WriteBlock a where
    textual :: a -> String
    textual = numerical
    numerical :: a -> String
    numerical = textual
    width :: a -> Int
    width = const 0

class ReadBlock a where
    number :: a -> Integer
    parseTextual :: Casing -> Parser a
    parseTextual = const $ fail "block can not be parsed textually"
    parseNumerical :: Padding -> Parser a
    parseNumerical = const $ fail "block can not be parsed numerically"

data WriteStyles = forall a. WriteBlock a => Write [a]

type ParseBlock = (Padding -> Parser Integer, Casing -> Parser Integer)
type ReadStyles = [ParseBlock]

blockParser :: forall a. ReadBlock a => ParseBlock
blockParser _ = (fmap number . num, fmap number . txt) where
    num = parseNumerical :: Padding -> Parser a
    txt = parseTextual :: Casing -> Parser a
