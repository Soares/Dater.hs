{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Write
    ( OutSection(Out, OutIn)
    , writeFormat
    , writeFormatIn
    , writeSpec
    , writeSpecIn
    , Formatter(..)
    , Formattable(..)
    , FormattableIn(..)
    , force -- TODO: extract
    ) where
import Control.Applicative
import Data.Char
import Data.Locale
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Format.Parse (loadFormat)
import Text.ParserCombinators.Parsec (ParseError)
import Text.Format.Table
    ( Style(..)
    , Casing(..)
    , Padding(..)
    , Format(..)
    , Section(..)
    , Spec
    )

data OutSection x
    = forall a. Formattable a => Out a
    | forall b. FormattableIn b x => OutIn b

writeFormat :: forall f x. (Format f, Formatter x f)
    => f -> String -> x
    -> Either ParseError String
writeFormat = writeFormatIn Nothing

writeFormatIn :: forall f x. (Format f, Formatter x f)
    => Maybe (Locale x)
    -> f -> String -> x
    -> Either ParseError String
writeFormatIn loc _ str x = writeSpecIn loc x <$> parsed where
    parsed = loadFormat str :: Either ParseError (Spec f)

writeSpec :: forall f x. (Format f, Formatter x f) => x -> Spec f -> String
writeSpec = writeSpecIn Nothing

writeSpecIn :: forall x f. (Format f, Formatter x f)
    => Maybe (Locale x)
    -> x -> Spec f
    -> String
writeSpecIn loc x = concatMap toStr where
    toStr :: Either (Section f) String -> String
    toStr (Right s) = s
    toStr (Left sec) = format c p w str where
        Section tgt sty p c alt = sec
        elm = formattable x tgt
        w = force 0 alt $ case elm of
            Out o -> widths o
            OutIn o -> widthsIn loc o
        str = render loc sty alt elm

force :: a -> Int -> [a] -> a
force a = (fromMaybe <$> fallback <*>) . atIndex where
    atIndex _ [] = Nothing
    atIndex 0 (x:_) = Just x
    atIndex n (_:xs) = atIndex (pred n) xs
    fallback = fromMaybe a . listToMaybe

render :: forall x. Maybe (Locale x) -> Style -> Int -> OutSection x -> String
render loc sty i sec = force "" i $ case (sty, sec) of
    (Name, Out o) -> names o
    (Number, Out o) -> numbers o
    (Name, OutIn o) -> namesIn loc o
    (Number, OutIn o) -> numbersIn loc o

format :: Casing -> Padding -> Int -> String -> String
format Normal None _ str = str
format Normal (Yours _) 0 str = str
format Normal (Yours c) w str = pad c w str
format Normal (Exactly c w) _ str = pad c w str
format Upper p d s = map toUpper $ format Normal p d s
format Lower p d s = map toLower $ format Normal p d s
format Inverted p d s = map invert $ format Normal p d s
    where invert c = if isUpper c then toLower c else toUpper c

pad :: Char -> Int -> String -> String
pad c w "" = replicate w c
pad c w str@(x:xs)
    | w <= 1 = str
    | all isDigit (c:xs) = x : pad c (w-1) xs
    | otherwise = replicate (w - length str) c ++ str

class Formatter x f where
    formattable :: x -> f -> OutSection x

class Formattable x where
    numbers :: x -> [String]
    names :: x -> [String]
    names = numbers
    widths :: x -> [Int]
    widths = const [0]

class FormattableIn a x where
    numbersIn :: Maybe (Locale x) -> a -> [String]
    namesIn :: Maybe (Locale x) -> a -> [String]
    widthsIn :: Maybe (Locale x) -> a -> [Int]
    widthsIn = const $ const [0]

instance Formattable Integer where
    numbers = pure . show
instance Formattable Int where
    numbers = pure . show
instance Formattable (Integer, String) where
    numbers = pure . show . fst
    names = pure . snd
instance Formattable String where
    numbers = const []
    names = pure
