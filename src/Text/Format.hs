{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Format where
import Control.Arrow
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Chunk

class Format f where
    table :: Map Char (Either (f, Style) String)

class Format f => Formatter x f where
    target :: f -> x -> OutBlock

class Formattable a where
    name :: a -> String
    number :: a -> Integer
    abbreviation :: Int -> a -> String
    abbreviation n = take n . name
    qualifier :: a -> String
    qualifier = const ""

expand :: forall f. Format f => String -> (Map Char (f, Style), String)
expand = foldr handle (Map.empty, "") where
    tbl = table :: Map Char (Either (f, Style) String)
    handle c (m, s) = case Map.lookup c tbl of
        Nothing -> (m, s ++ [c])
        Just (Left fy) -> (Map.insert c fy m, s)
        Just (Right x) -> (m, s ++ x)

deconstruct :: forall f. Format f => String -> [Either (f, Chunk) String]
deconstruct vanilla = undefined where
    (dict, str) = expand vanilla :: (Map Char (f, Style), String)

display :: forall x f. Formatter x f => f -> String -> x -> String
display f str x = foldr apply "" sections where
    sections = deconstruct str :: [Either (f, Chunk) String]
    apply (Left (t, c)) accum = accum ++ format c (target t x)
    apply (Right s) accum = accum ++ s

format :: Chunk -> OutBlock -> String
format c a = case style c of
    Number -> prepare c $ show $ number a
    Name -> prepare c $ name a
    (Abbreviation i) -> prepare c $ abbreviation i a
    Qualifier -> prepare c $ qualifier a

prepare :: Chunk -> String -> String
prepare c str = caseify $ pad ++ str where
    pad = replicate (width c - length str) (padding c)
    caseify = case casing c of
        Normal -> id
        Upper -> map toUpper
        Lower -> map toLower
        Inverted -> map invert
        where invert h = if isUpper h then toLower h else toUpper h

data OutBlock = forall x. Formattable x => Out x

instance Formattable OutBlock where
    name (Out s) = name s
    number (Out s) = number s
    abbreviation i (Out s) = abbreviation i s
    qualifier (Out s) = qualifier s

instance Formattable Integer where
    name = show
    number = id

instance Formattable Int where
    name = show
    number = toInteger
