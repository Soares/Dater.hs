{-# LANGUAGE RankNTypes #-}
module Text.Format.Table
    ( Style(..)
    , Casing(..)
    , Directive(..)
    , Options(..)
    , Section(..)
    , Format(..)
    , defaults
    , flatten
    ) where
import Data.Map (Map)
import qualified Data.Map as Map

data Style
    = Number
    | Name
    | Abbreviation Int
    deriving (Eq, Ord, Read, Show)

data Casing
    = Normal
    | Upper
    | Lower
    | Inverted
    deriving (Eq, Ord, Enum, Read, Show)

data Directive f
    = Directive f Style
    | Shortcut [Either (Options, Directive f) String]

data Options = Options
    { padding   :: (Char, Int)
    , casing    :: Casing
    , alternate :: Int
    }

data Section f = Section
    { target  :: Format f => f
    , style   :: Style
    , options :: Options
    }

class Format f where
    table :: Map Char (Directive f)

defaults :: Options
defaults = Options ('0', 0) Normal 0

flatten :: [Either (Options, Directive f) String] -> [Either (Section f) String]
flatten [] = []
flatten (Right x : xs) = Right x : flatten xs
flatten (Left (o, d) : xs) = reduce o d ++ flatten xs where
    reduce :: Options -> Directive f -> [Either (Section f) String]
    reduce o (Directive f y) = [Left $ Section f y o]
    reduce o (Shortcut ds) = map (update o) (flatten ds)
    update o (Left sec) = Left sec{options=o}
    update _ str = str
