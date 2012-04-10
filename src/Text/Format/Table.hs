{-# LANGUAGE RankNTypes #-}
module Text.Format.Table
    ( Spec
    , Style(..)
    , Casing(..)
    , Padding(..)
    , Directive(..)
    , Options(..)
    , Section(..)
    , Format(..)
    , defaults
    , flatten
    , decrease
    , derive
    , reoption
    , force
    ) where
import Data.Maybe (fromMaybe)
import Data.Map (Map)

type Spec f = [Either (Section f) String]

data Style = Number | Name
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

data Padding
    = None
    | Yours Char
    | Exactly Char Int
    deriving (Eq, Ord, Read, Show)

decrease :: Padding -> Padding
decrease (Exactly c 0) = Exactly c 0
decrease (Exactly c w) = Exactly c $ pred w
decrease p = p


data Options = Options
    { pad :: Maybe Padding
    , txt :: Maybe Casing
    , alt :: Maybe Int
    } deriving (Eq, Read, Show)

data Section f = Section
    { target    :: f
    , style     :: Style
    , padding   :: Padding
    , casing    :: Casing
    , alternate :: Int
    } deriving (Eq, Read, Show)

derive :: f -> Style -> Options -> Section f
derive f y (Options p c a) = Section
    { target = f
    , style = y
    , padding = fromMaybe None p
    , casing = fromMaybe Normal c
    , alternate = fromMaybe 0 a
    }

reoption :: Section f -> Options -> Section f
reoption sec (Options p c a) = sec
    { padding = fromMaybe (padding sec) p
    , casing = fromMaybe (casing sec) c
    , alternate = fromMaybe (alternate sec) a
    }

class (Eq f, Ord f) => Format f where
    table :: Map Char (Directive f)

defaults :: Options
defaults = Options Nothing Nothing Nothing

flatten :: [Either (Options, Directive f) String] -> Spec f
flatten [] = []
flatten (Right x : xs) = Right x : flatten xs
flatten (Left (o, d) : xs) = reduce o d ++ flatten xs where
    reduce :: Options -> Directive f -> Spec f
    reduce opts (Directive f y) = [Left $ derive f y opts]
    reduce opts (Shortcut secs) = map (update opts) secs
    update opts (Left sec) = Left sec
        { padding = fromMaybe (padding sec) (pad opts)
        , casing = fromMaybe (casing sec) (txt opts)
        , alternate = fromMaybe (alternate sec) (alt opts)
        }
    update _ str = str

-- | Helper function to select readers/writers, etc.
-- | Attempts to find a value at a certain index in a list.
-- | If the index is not present, falls back to the first element in the list.
-- | If the list is empty, falls back to a given default value.
-- | TODO: probably should be in list utils somewhere.
force :: a -> Int -> [a] -> a
force a _ [] = a
force _ i xs = (concat $ repeat xs) !! i
