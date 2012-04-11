{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DateTime.Gregorian.TimeZones
    ( TimeZone(..)
    , ShowStyle(..)
    , Locale(..)
    , utcOffset
    , universal
    , showTimeZone
    , zoneNames
    , timeZones
    , localTimeZoneParsers
    , localTimeZoneStyles
    , nonlocalTimeZoneParsers
    , nonlocalTimeZoneStyles
    ) where
import Control.Arrow
import Control.Applicative
import Data.DateTime.Gregorian.Date
import Data.DateTime.Gregorian.Time
import Data.DateTime.Gregorian.Places
import Data.Locale
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.Normalize
import Text.Format.Write
import Text.Format.Read
import Text.Format.Table (Padding(..), decrease)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec hiding ((<|>))

data instance Locale TimeZone = At Place Date Time

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

-- TODO: abstract similarities between local...
nonlocalTimeZoneStyles :: TimeZone -> Writers
nonlocalTimeZoneStyles tz = disjointBlock nums names where
    nums = map (`showTimeZone` tz) [minBound..maxBound]
    names = catMaybes [code tz, name tz, Just $ showTimeZone HourMinute tz]

-- TODO: move to list utils
flatten2 :: [(a, a)] -> [a]
flatten2 (x:xs) = fst x : snd x : flatten2 xs
flatten2 [] = []

localTimeZoneStyles :: Locale TimeZone -> TimeZone -> Writers
localTimeZoneStyles loc tz = djb nums names where
    djb = disjointBlock :: [String] -> [String] -> Writers
    nums = map (`showTimeZone` tz) [minBound..maxBound]
    names = flatten2 $ zoneNames loc tz :: [String]

nonlocalTimeZoneParsers :: [ParseBlock]
nonlocalTimeZoneParsers = zip ps (repeat err) where
    ps = map p [minBound..maxBound]
    p = (fmap toDecimal .) . parseTimeZone . toEnum
    err _ = fail "time zone names can not be parsed without a locale"

localTimeZoneParsers :: Locale TimeZone -> [ParseBlock]
localTimeZoneParsers loc = disjointParser ps ns where
    ps = map p [minBound..maxBound]
    p = (fmap toDecimal .) . parseTimeZone . toEnum
    zs = concatMap flat $ timeZones loc
    flat (tz, short, long) = [(short, tz), (long, tz)]
    makeParser (n, tz) c = try ((stringParser c n) *> pure (toDecimal tz))
    ns = map makeParser zs

zoneNames :: Locale TimeZone -> TimeZone -> [(String, String)]
zoneNames = undefined

timeZones :: Locale TimeZone -> [(TimeZone, String, String)]
timeZones = undefined

showTimeZone :: ShowStyle -> TimeZone -> String
showTimeZone sty tz = let
    s = case compare (offset tz) 0 of
        EQ -> '±'
        LT -> '-'
        GT -> '+'
    fmt = case sty of
        Numeric -> printf "%c%02d%02d"
        HourMinute -> printf "%c%02d:%02d"
        HourMinuteSecond -> printf "%c%02d:%02d:00"
        Minimal -> if minute tz == 0
            then \a b _ -> printf "%c%02d" a b
            else printf "%c%02d:%02d"
    in fmt s (hour tz) (minute tz)

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
