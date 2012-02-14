{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Halp where
import Calendar
import Control.Applicative
import Data.Ratio hiding ((%))
import Data.Vec ( Vec, VecList, Fold, Head, Tail, Map, ZipWith, (:.)(..), toList, fromList )
import Data.Vec.Nat
import qualified Data.Vec as Vec
import Prelude hiding ((!!))
import Range hiding (mod)
import Utils

data Common n = Cal
    { months        :: Year -> Range
    , days          :: Year -> Month -> Range
    , timeSplits    :: (NList n Integer v) => YMD -> v
    , beginning     :: Rational
    }

data (NList n Integer v) => DateTime n v = DateTime
    { year   :: Year
    , month  :: Month
    , day    :: Day
    , time   :: v
    , detail :: Detail
    }

class (Vec n a v, Fold v a, VecList a v, ZipWith a a a v v v) => NList n a v
instance NList N1 a (a :. ())
instance NList N2 a (Vec.Vec2 a)
instance NList N3 a (Vec.Vec3 a)


-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: Common n -> Year -> Integer
daysInYear f y = sum $ map (size . days f y) $ elems $ months f y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: NList n Integer v => Common n -> YMD -> Integer
timeUnitsPerDay f = Vec.product . timeSplits f

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dateOfYear :: Common n -> Year -> Integer -> YMD
dateOfYear f y n | n < 0 = dateOfYear f (y-1) (n + daysInYear f (y-1))
                 | n >= daysInYear f y = dateOfYear f (y+1) (n - daysInYear f y)
                 | otherwise = dayEnum !! n where
    dayEnum = [(y, m, d) | m <- elems $ months f y, d <- elems $ days f y m]

-- | Given YMD and the 'time' portion of a Date, determine how
-- | to split up the time portion
timeOfDay :: NList n Integer v => Common n -> YMD -> Integer -> v
timeOfDay f ymd t = fromList . split . reverse . toList $ timeSplits f ymd where
    split = snd . foldl split' (t :: Integer, [])
    split' (n, xs) x = (div n x, mod n x : xs)

-- | The year, month, and day of a date rational
largePart :: Common n -> Rational -> YMD
largePart f r | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (daysInYear f) ys)
    cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
    containing = head . filter ((abs d <) . snd)
    from (y, t) = dateOfYear f y n where n = (d-t) + daysInYear f y
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: NList n Integer v => Common n -> Rational -> v
smallPart f r = timeOfDay f (largePart f r) (timePart f r) where

-- | Just the part of the rational relevant to the day
dayPart :: NList n Integer v => Common n -> Rational -> Rational
dayPart f r = leftover r * toRational (timeUnitsPerDay f $ largePart f r)

-- | Just the part of the rational relevant to the time
timePart :: NList n Integer v => Common n -> Rational -> Integer
timePart f = floor . dayPart f

-- | Any part of the rational so small that it's not relevant to the time
detailPart :: NList n Integer v => Common n -> Rational -> Detail
detailPart f = leftover . dayPart f

-- | Split a rational into its component parts
breakDown :: NList n Integer v => Common n -> Rational -> DateTime n v
breakDown f r = DateTime y m d (smallPart f r) (detailPart f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: NList n Integer v => Common n -> DateTime n v -> Rational
rebuild f (DateTime y m d t x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x % timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: Common n -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: Common n -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: Common n -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: NList n Integer v => Common n -> YMD -> v -> Rational
dayFraction f ymd ts = timeInSeconds % timeUnitsPerDay f ymd where
    timeInSeconds = Vec.sum $ Vec.zipWith (*) ts mods
    -- TODO: mods = drop 1 $ prods $ timeSplits f ymd
    mods = timeSplits f ymd

instance (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) => Calendar (Common n) where
    data Delta (Common n) = Delta
        { dYear   :: Maybe Year
        , dMonth  :: Maybe Month
        , dDay    :: Maybe Day
        , dTime   :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) => v
        , dDetail :: Maybe Detail
        }

    display _ _ = [] -- TODO
    parse _ _ = [] -- TODO

    plus = change madd
    minus = change msub
    clobber = change mright

    normalize c r = r + beginning c
    denormalize c r = r - beginning c

-- TODO
instance Read (Common n) where
    readsPrec _ _ = []
instance Show (Common n) where
    show _ = []
instance Read (Delta (Common n)) where
    readsPrec _ _ = []
instance Show (Delta (Common n)) where
    show _ = []

type Operation = Integer -> Maybe Integer -> Integer

madd, msub, mright :: Operation
madd i = maybe i (i +)
msub i = maybe i (i -)
mright i = maybe i id

operation :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) =>
             (Integer -> Maybe Integer -> Integer) ->
             DateTime n u -> Delta (Common n) -> DateTime n u
operation op (DateTime y m d (ts :: u) x) (Delta my mm md (mt :: v) mx) = DateTime
    (op y my) (op m mm) (op d md)
    (Vec.zipWith op ts mt)
    (op (numerator x) (numerator <$> mx) % op (denominator x) (denominator <$> mx))

change :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) =>
          (Integer -> Maybe Integer -> Integer) ->
          Common n -> Rational -> Delta (Common n) -> Rational
change op c r = rebuild c . operation op (breakDown c r)

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake f rel = rebuild f (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}

