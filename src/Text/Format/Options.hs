{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Text.Format.Options where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- Alternate, Casing, Padding, Width
type Options = (Int, Casing, Char, Int)
defaults = (0, Normal, '0', 0)

data Casing
    = Normal
    | Upper
    | Lower
    | Inverted
    deriving (Eq, Ord, Enum, Read, Show)

data Chunk = Chunk
    { alternate :: Int
    , width     :: Int
    , padding   :: Char
    , casing    :: Casing
    }

defaultChunk :: Chunk
defaultChunk = Chunk
  { alternate = 0
  , width = 0
  , padding = '0'
  , casing = Normal
  }

{-

class Loadable a where
    names :: a -> [String]
    names = const []
    digits :: a -> Int
    digits = const 0
    abbreviations :: a -> Int -> [String]
    abbreviations a i = map (take i) $ names a
    qualifiers :: a -> [String]
    qualifiers = const []
    qualify :: String -> a -> a
    qualify = const id

instance Loadable Integer
instance Loadable Int



data Format = forall x. Formatable x => Format x
data Section = forall x. Loadable x => Section x

instance Formatable Format where
    name (Format s) = name s
    number (Format s) = number s
    abbreviation i (Format s) = abbreviation i s
    qualifier (Format s) = qualifier s

class Loader x f where
    load :: Ord f => Map f Integer -> x
    loader :: x -> f -> Section

class Formatter x f where
    target :: f -> x -> Format

breakAndRead = undefined
deconstruct :: Formatter x f => x -> f -> String -> [Either String (f, Chunk)]
deconstruct = undefined

build :: forall d f. (Ord f, Loader d f) => f -> String -> d
build f str = let
    d = undefined :: d
    chunks = breakAndRead d f str
    dict :: Map f Integer
    dict = foldr apply Map.empty chunks
    apply :: (f, Either Integer String) -> Map f Integer -> Map f Integer
    apply (t, Left i) m = Map.insert t i m
    apply (t, Right q) m = let
        -- TODO: what if it's not in there yet?
        -- qualifier = loader d t
        -- updated = qualify q qualifier
        updated = undefined :: Integer
        in Map.insert t updated m
    in load dict

display :: forall x f. Formatter x f => f -> String -> x -> String
display f str x = let
    chunks = deconstruct x f str
    built = foldr apply "" chunks
    apply :: Either String (f, Chunk) -> String -> String
    apply (Left s) tot = tot ++ s
    apply (Right (t, c)) tot = tot ++ format c (tar t x)
    tar = target :: f -> x -> Format
    in foldr apply "" chunks

        -}
