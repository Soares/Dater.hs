{-# LANGUAGE RankNTypes #-}
module Text.Format.Table
    ( Spec
    , Style(..)
    , Casing(..)
    , Directive(..)
    , Options(..)
    , Section(..)
    , Format(..)
    , defaults
    , flatten
    ) where
import Data.Map (Map)

type Spec f = [Either (Section f) String]

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
    | Shortcut (Spec f)

data Options = Options
    { padding   :: (Char, Int)
    , casing    :: Casing
    , alternate :: Int
    } deriving (Eq, Read, Show)

data Section f = Section
    { target  :: f
    , style   :: Style
    , options :: Options
    } deriving (Eq, Read, Show)

class (Eq f, Ord f) => Format f where
    table :: Map Char (Directive f)

defaults :: Options
defaults = Options ('0', 1) Normal 0

flatten :: [Either (Options, Directive f) String] -> Spec f
flatten [] = []
flatten (Right x : xs) = Right x : flatten xs
flatten (Left (o, d) : xs) = reduce o d ++ flatten xs where
    reduce :: Options -> Directive f -> Spec f
    reduce opts (Directive f y) = [Left $ Section f y opts]
    reduce opts (Shortcut secs) = map (update opts) secs
    update opts (Left sec) = Left sec{options=opts}
    update _ str = str
