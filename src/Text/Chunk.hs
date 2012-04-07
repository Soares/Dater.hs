module Text.Chunk where
import Data.Char

data Style
    = Number
    | Name
    | Abbreviation Int
    | Qualifier
    deriving (Eq, Ord, Read, Show)

data Casing
    = Normal
    | Upper
    | Lower
    | Inverted
    deriving (Eq, Ord, Enum, Read, Show)

data Chunk = Chunk
    { style   :: Style
    , width   :: Int
    , padding :: Char
    , casing  :: Casing
    }

format :: Displayable a => Chunk -> a -> String
format c a = case style c of
    Number -> prepare c $ show $ number a
    Name -> prepare c $ name a
    (Abbreviation i) -> prepare c $ abbreviation i a
    Qualifier -> prepare c $ qualifier a

prepare :: Chunk -> String -> String
prepare c str = caseify $ pad ++ str where
    pad = take (width c - length str) $ repeat $ padding c
    caseify = case casing c of
        Normal -> id
        Upper -> map toUpper
        Lower -> map toLower
        Inverted -> map invert where
    invert h = if isUpper h then toLower h else toUpper h

class Displayable a where
    name :: a -> String
    number :: a -> Integer
    abbreviation :: Int -> a -> String
    abbreviation n = take n . name
    qualifier :: a -> String
    qualifier = const ""
