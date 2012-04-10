{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Write
    ( Writers(..)
    , WriteBlock(..)
    , disjointBlock
    , writeFormat
    , writeFormatIn
    , writeSpec
    , writeSpecIn
    , out
    , Formatter(..)
    ) where
import Control.Applicative
import Data.Char
import Data.Locale
import Text.Format.Parse (loadFormat)
import Text.ParserCombinators.Parsec (ParseError)
import Text.Format.Table
    ( Style(..)
    , Casing(..)
    , Padding(..)
    , Format(..)
    , Section(..)
    , Spec
    , force
    )


data Writers = forall a. WriteBlock a => Write [a]

out :: forall a. WriteBlock a => a -> Writers
out = Write . pure

class WriteBlock a where
    textual :: a -> String
    textual = numerical
    numerical :: a -> String
    numerical = textual
    width :: a -> Int
    width = const 0

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
        elm = formattable loc x tgt
        w = force 0 alt $ case elm of (Write o) -> map width o
        str = render sty alt elm

render :: Style -> Int -> Writers -> String
render sty i sec = force "" i $ case (sty, sec) of
    (Name, Write o) -> map textual o
    (Number, Write o) -> map numerical o

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

disjointBlock :: forall a b. WriteBlock (a, b) => [a] -> [b] -> Writers
disjointBlock a b = Write $ zip (cycle a) (cycle b)

class Formatter x f where
    formattable :: Maybe (Locale x) -> x -> f -> Writers

instance WriteBlock Integer where
    numerical = show

instance WriteBlock Int where
    numerical = show

instance WriteBlock String where
    textual = id

instance WriteBlock (Int, String) where
    numerical = numerical . fst
    textual = snd

instance WriteBlock (Integer, String) where
    numerical = numerical . fst
    textual = snd

instance WriteBlock (String, String) where
    numerical = fst
    textual = snd
