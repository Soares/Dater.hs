{-# LANGUAGE FlexibleInstances #-}
module Data.DateTime.Gregorian.TimeZones
    ( TimeZone(..)
    , ShowStyle(..)
    , utcOffset
    , universal
    , showTimeZone
    , zoneNames
    , timeZones
    ) where
import Control.Arrow
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.Normalize
import Text.Format.Write
import Text.Format.Read
import Text.Format.Table (Padding(..), decrease)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec hiding ((<|>))

-- TODO
type Date = ()

data Place
    = Africa
    | Antarctica
    | Asia
    | Atlantic
    | Australia
    | Caribbean
    | CentralAmerica
    | Everywhere
    | Europe
    | IndianOcean
    | Military
    | NorthAmerica
    | Pacific
    | SouthAmerica
    deriving (Eq, Ord, Read, Show, Enum)

data ShowStyle
    = Numeric
    | HourMinute
    | HourMinuteSecond
    | Minimal deriving (Eq, Ord, Enum, Read, Show, Bounded)

data TimeZone = TZ
    { code   :: Maybe String
    , name   :: Maybe String
    , offset :: Int
    }

instance Eq TimeZone where (==) = (==) `on` offset
instance Ord TimeZone where (<=) = (<=) `on` offset

simple :: Integral a => a -> TimeZone
simple = TZ Nothing Nothing . fromIntegral

hour :: TimeZone -> Int
hour = (`div` 60) . offset

minute :: TimeZone -> Int
minute = (`mod` 60) . offset

toDecimal :: TimeZone -> Integer
toDecimal tz = toInteger $ (100 * hour tz) + (minute tz)

fromDecimal :: Integer -> TimeZone
fromDecimal i = simple $ ((i `div` 100) * 60) + (i `mod` 100)

fromHourMinute :: (Integral a, Integral b) => a -> b -> TimeZone
fromHourMinute h m = simple $ (60 * fromIntegral h) + m

utcOffset :: TimeZone -> Int
utcOffset = (60 *) . offset

universal :: TimeZone
universal = simple (0::Int)

instance Show TimeZone where show = showTimeZone HourMinute

instance Normalize TimeZone where
    isNormal = (== universal)
    normalize = overflow &&& const universal
    overflow = utcOffset

instance Formattable TimeZone where
    numbers tz = map (`showTimeZone` tz) [minBound..maxBound]
    names tz = catMaybes [code tz, name tz, Just $ showTimeZone HourMinute tz]

instance Formattable (TimeZone, Place, Date) where
    numbers (tz, _, _) = map (`showTimeZone` tz) [minBound..maxBound]
    names (tz, p, d) = flatten2 $ zoneNames tz p d

instance Loadable TimeZone where
    parseNumber _ i p = toDecimal <$> parseTimeZone (toEnum i) p
    parseName tz _ _ = parseNumber tz 0 None

instance Loadable (TimeZone, Place, Date) where
    parseNumber _ i p = toDecimal <$> parseTimeZone (toEnum i) p
    parseName x@(_, p, d) i c = force fb i $ map makeParser zs where
        fb = parseNumber x 0 None
        zs = concatMap flat $ timeZones p d
        flat (tz, short, long) = [(short, tz), (long, tz)]
        makeParser (n, tz) = try ((cased c n) *> pure (toDecimal tz))

zoneNames :: TimeZone -> Place -> Date -> [(String, String)]
zoneNames = undefined

timeZones :: Place -> Date -> [(TimeZone, String, String)]
timeZones = undefined

flatten2 :: [(a, a)] -> [a]
flatten2 (x:xs) = fst x : snd x : flatten2 xs
flatten2 [] = []

showTimeZone :: ShowStyle -> TimeZone -> String
showTimeZone sty tz = let
    s = case compare (offset tz) 0 of
        EQ -> '±'
        LT -> '-'
        GT -> '+'
    out = case sty of
        Numeric -> printf "%c%02d%02d"
        HourMinute -> printf "%c%02d:%02d"
        HourMinuteSecond -> printf "%c%02d:%02d:00"
        Minimal -> if minute tz == 0
            then \a b _ -> printf "%c%02d" a b
            else printf "%c%02d:%02d"
    in out s (hour tz) (minute tz)

dropChunk :: Padding -> Padding
dropChunk = decrease . decrease . decrease

parseTimeZone :: ShowStyle -> Padding -> Parser TimeZone
parseTimeZone Numeric p = fromDecimal <$> sizedSignedParser 5 p
parseTimeZone HourMinute p = fromHourMinute <$> h <*> m where
    h = sizedSignedParser 3 (dropChunk p)
    m = char ':' *> (read <$> count 2 digit) :: Parser Int
parseTimeZone HourMinuteSecond p =
    parseTimeZone HourMinute (dropChunk p) <* string ":00"
parseTimeZone Minimal p =
    try (parseTimeZone HourMinute p)
    <|> (fromHourMinute <$> (sizedSignedParser 3 p) <*> pure (0::Int))
