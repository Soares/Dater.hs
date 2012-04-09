{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Write where
import Control.Applicative
import Data.Char
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

data OutSection = forall x. Formattable x => Out x

writeFormat :: forall f x. (Format f, Formatter x f)
    => f -> String -> x -> Either ParseError String
writeFormat _ str x = writeSections x <$> parsed where
    parsed = loadFormat str :: Either ParseError (Spec f)

writeSections :: forall f x. (Format f, Formatter x f) => x -> Spec f -> String
writeSections x = concatMap toStr where
    toStr :: Either (Section f) String -> String
    toStr (Right s) = s
    toStr (Left sec) = format c p dig str where
        Section tgt sty p c alt = sec
        elm = formattable x tgt
        dig = width elm
        str = render sty alt elm

force :: Int -> a -> [a] -> a
force _ a [] = a
force 0 _ (x:_) = x
force i a (_:xs) = force (pred i) a xs

render :: Style -> Int -> OutSection -> String
render Name i o = force i (name o) (names o)
render Number i o = force i (show $ number o) (numbers o)

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
    | x `elem` "+-±" = x : pad c (w-1) xs
    | otherwise = replicate (w - length str) c ++ str

class Formatter x f where
    formattable :: x -> f -> OutSection

class Formattable x where
    number :: x -> Integer
    number = read . (!! 0) . numbers
    numbers :: x -> [String]
    numbers = pure . show . number
    name :: x -> String
    name = (!! 0) . names
    names :: x -> [String]
    names = pure . name
    width :: x -> Int
    width = const 0

instance Formattable OutSection where
    name (Out s) = name s
    names (Out s) = names s
    number (Out s) = number s
    numbers (Out s) = numbers s
    width (Out s) = width s

instance Formattable Integer where
    number = id
    name = show . number
instance Formattable Int where
    number = toInteger
    name = show . number
instance Formattable (Integer, String) where
    number = fst
    name = snd
instance Formattable String where
    number s = error $ "can't write string \""++s++"\" to number"
    name = id
