{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Format.Write where
import Data.Char
import Text.Format.Table

data OutSection = forall x. Formattable x => Out x

writeFormat :: forall f x. (Format f, Formatter x f)
    => x -> [Either (Section f) String] -> String
writeFormat x = concatMap toStr where
    toStr :: Either (Section f) String -> String
    toStr (Right s) = s
    toStr (Left sec) = format cas pad dig str where
        Section tgt sty opt = sec
        Options pad cas alt = opt
        elm = formattable x tgt alt
        dig = digits elm
        str = render sty elm

render :: Style -> OutSection -> String
render Name = name
render Number = show . number
render (Abbreviation i) = abbreviation i

format :: Casing -> (Char, Int) -> Int -> String -> String
format Normal (_, 0) _ str = str
format Normal (c, 1) 0 str = str
format Normal (c, 1) d str = replicate (d - length str) c ++ str
format Normal (c, n) _ str = replicate (n - length str) c ++ str
format Upper p d s = map toUpper $ format Normal p d s
format Lower p d s = map toLower $ format Normal p d s
format Inverted p d s = map invert $ format Normal p d s
    where invert c = if isUpper c then toLower c else toUpper c

class Formatter x f where
    formattable :: x -> f -> Int -> OutSection

class Formattable x where
    number :: x -> Integer
    name :: x -> String
    name = show . number
    abbreviation :: Int -> x -> String
    abbreviation i = take i . name
    digits :: x -> Int
    digits = const 0

instance Formattable OutSection where
    name (Out s) = name s
    number (Out s) = number s
    abbreviation i (Out s) = abbreviation i s
    digits (Out s) = digits s

instance Formattable Integer where number = id
instance Formattable Int where number = toInteger
