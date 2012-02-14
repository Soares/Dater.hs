{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Common
    ( Common(..)
    , Year
    , Month
    , Day
    , YMD
    , Detail
    -- , breakDown
    -- , rebuild
    ) where
import Calendar
import Control.Applicative
import Data.Maybe
import Data.Ratio hiding ((%))
import Data.Vec ( Vec, VecList, Fold, Head, Tail, Map, ZipWith, (:.)(..), toList, fromList )
import Data.Vec.Nat
import qualified Data.Vec as Vec
import Prelude hiding ((!!))
import Range hiding (mod)
import Utils

-- | Dates are measured as a Rational, measuring days since the 'beginning'
-- | moment. Note that events may happen before the 'beginning' moment as
-- | rationals may be negative.
-- |
-- | Days are a good choice of measure here because days don't easily
-- | divide years (see pre-gregorian callendars) but most earthlike dates
-- | can be related fairly easily through days.
-- |
-- | An earthlike date consists of the following:
-- | An outer unit
-- type Year = Integer
-- | A primary unit, dependent upon the secondary unit
-- type Month = Integer
-- | A secondary unit
-- type Day = Integer
-- | (Year, Month, and Day can be abbreviated YMD)
-- type YMD = (Year, Month, Day)
-- | Leftover detail
type Detail = Rational
-- |
-- | Further, it is assumed that the primary and secondary units are
-- | interlinked, i.e. the primary unit is dependant upon the secondary.
-- | In other words, you don't say that it is day 32, you say that it is
-- | Month 2, day 1.
-- |
-- | This constraint can be relaxed by having 'only one month' or by
-- | having 'only one day per month' or by having months that generate
-- | uniform days, depending upon how you want the constraint relaxed.
-- |
-- | Again further, it is assumed that the time splits depend only on the
-- | Outer and primary units (since the secondary unit depends upon the primary
-- | unit). In other words, the number of hours in a day is allowed to vary
-- | by time of year (i.e. for calendars that measure daytime hours as a
-- | tenth-portion of daylight on earth; hour length would change throughout
-- | the year). This constraint may be relaxed by making chunks ignore
-- | the year and month parameters.
-- |
-- | You need only supply an Date and this module will do the rest.

class (Fold v i) => Earthlike dt v i where
    data DateTime v i

    -- | The number of days in a year.
    -- | Defaults to counting the number of days in all months that year.
    daysInYear      :: dt v -> i -> i
    -- | Given a year and the 'day' portion of a Date, determine the Month/Day
    -- | pair of the date.
    dateOfYear      :: dt v -> i -> i -> (i,i,i)
    -- | The number of 'seconds' in a day.
    timeUnitsPerDay :: dt v -> (i,i,i) -> i
    -- | Given YMD and the 'time' portion of a Date, determine how
    -- | to split up the time portion
    timeOfDay       :: dt v -> (i,i,i) -> i -> v

    -- | Just the part of the rational relevant to the day
    dayPart         :: dt v -> Rational -> Rational
    -- | The year, month, and day of a date rational
    largePart       :: dt v -> Rational -> (i,i,i)
    -- | Just the part of the rational relevant to the time
    timePart        :: dt v -> Rational -> i
    -- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
    smallPart       :: dt v -> Rational -> v
    -- | Any part of the rational so small that it's not relevant to the time
    detailPart      :: dt v -> Rational -> Rational

    -- | Split a rational into its component parts
    breakDown       :: dt v -> Rational -> DateTime v i
    -- | Rebuild a rational from its component parts
    rebuild         :: dt v -> DateTime v i -> Rational

data (Fold (v i) i, Integral i) => Common v i = Cal
    { months        :: i -> Range
    , days          :: i -> i -> Range
    , timeSplits    :: (i, i, i) -> v i
    , beginning     :: Rational
    }

instance (Fold (v i) i, Integral i) => Earthlike Common v i where
    data DateTime v i = DateTime
        { year   :: i
        , month  :: i
        , day    :: i
        , time   :: (v i)
        , detail :: Rational
        }

    daysInYear c y = sum $ map (size . days c y) $ elems $ months c y

    timeUnitsPerDay c = Vec.product . timeSplits c

    dateOfYear c y n | n < 0 = dateOfYear c (y-1) (n + daysInYear c (y-1))
                     | n >= daysInYear c y = dateOfYear c (y+1) (n - daysInYear c y)
                     | otherwise = dayEnum !! n where
        dayEnum = [(y, m, d) | m <- elems $ months c y, d <- elems $ days c y m]

    timeOfDay c ymd n = snd $ Vec.foldr split (n, undefined) $ timeSplits c ymd where
        split _ _ = undefined
        -- TODO: split (n, xs) x = (div n x, mod n x :. xs)

    largePart c r | d < 0 = from . containing $ dayedYears [-1,-2..]
                  | otherwise = from . containing $ dayedYears [0..] where
        dayedYears ys = zip ys (cascade $ map (daysInYear c) ys)
        cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
        containing = head . filter ((abs d <) . snd)
        from (y, t) = dateOfYear c y n where n = (d-t) + daysInYear c y
        d = floor r

    smallPart c r = timeOfDay c (largePart c r) (timePart c r) where

    dayPart c r = leftover r * toRational (timeUnitsPerDay c $ largePart c r)

    timePart c = floor . dayPart c

    detailPart c = leftover . dayPart c

    breakDown c r = DateTime y m d (smallPart c r) (detailPart c r)
        where (y, m, d) = largePart c r

    rebuild c (DateTime y m d t x) = aa + bb + cc where
        aa = toRational $ numDays c (y, m, d)
        bb = dayFraction c (y, m, d) t
        cc = x % timeUnitsPerDay c (y, m, d)

        -- | Custom methods
        normalizeYM     :: Common v i -> i -> i -> (i,i)
        normalizeYMD    :: Common v i -> (i,i,i) -> (i,i,i)
        numDays         :: Common v i -> (i,i,i) -> i
        dayFraction     :: Common v i -> (i,i,i) -> (v i) -> Rational

        -- | Adjust the year and month until they are sane, i.e.
        -- | 12/-1 becomes 11/12
        normalizeYM c y m
            | months c y `contains` m = (y, m)
            | otherwise = normalizeYM c (y + delta) m'
            where (delta, m') = push m (around (months c) y)

        -- | Adjust the year, month, and day until it is sane, i.e.
        -- | 12/0/-1 becomes 11/12/30
        normalizeYMD c (y, m, d)
            | days c ny nm `contains` d = (ny, nm, d)
            | otherwise = normalizeYMD c (ny, nm + delta, d')
            where (delta, d') = push d (around (days c ny) nm)
                  (ny, nm) = normalizeYM c y m

        -- | Turn a YMD into the number of days since 'the beginning'
        numDays c ymd = ydays + mdays + ddays where
            ydays = sum $ map (daysInYear c) ys
            ys = if y >= 0 then [0..y-1] else [-1,-2..y]
            mdays = sum $ map (size . days c y) ms
            ms = filter (m >) (elems $ months c y)
            ddays = toInteger $ length $ filter (d >) (elems $ days c y m)
            (y, m, d) = normalizeYMD c ymd

        -- | Turn a Time into a fraction of a day
        dayFraction c ymd ts = timeInSeconds % timeUnitsPerDay c ymd where
            timeInSeconds :: Integer = Vec.sum $ Vec.zipWith (*) ts (prods splits)
            splits = timeSplits c ymd
            prods _ = undefined
            -- TODO: prods (_ :. xs) = Vec.product xs :. prods xs


class Maybeify a a'
instance Maybeify () ()
instance Maybeify (a :. ()) (Maybe a :. ())
instance (Maybeify v v') => Maybeify (a :. v) (Maybe a :. v')

instance (Fold (v i) i, Integral i) => Calendar (Common (v i) i) where
    data Delta (Common v i) = Delta
        { dYear   :: Maybe i
        , dMonth  :: Maybe i
        , dDay    :: Maybe i
        , dTime   :: v (Maybe i)
        , dDetail :: Maybe Rational
        }

    display _ _ = [] -- TODO
    parse _ _ = [] -- TODO

    plus = change madd
    minus = change msub
    clobber = change fromMaybe

    normalize c r = r + beginning c
    denormalize c r = r - beginning c

madd, msub :: (Integral i) => i -> Maybe i -> i
madd i = maybe i (i +)
msub i = maybe i (i -)

operation :: (Fold (v i) i, Integral i) =>
             (i -> Maybe i -> i) ->
             DateTime v i -> Delta (Common v i) -> DateTime v i
operation op (DateTime y m d ts x) (Delta my mm md mt mx) = DateTime
    (op y my) (op m mm) (op d md)
    (Vec.zipWith op ts mt)
    (op (numerator x) (numerator <$> mx)
     % op (denominator x) (denominator <$> mx))

change :: (Fold (v i) i, Integral i) =>
          (i -> Maybe i -> i) ->
          Common v i -> Rational -> Delta (Common v i) -> Rational
change op c r = rebuild c . operation op (breakDown c r)

-- TODO
instance Read (Common v i) where
    readsPrec _ _ = []
instance Show (Common v i) where
    show _ = []
instance Read (Delta (Common v i)) where
    readsPrec _ _ = []
instance Show (Delta (Common v i)) where
    show _ = []

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake c rel = rebuild c (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}
