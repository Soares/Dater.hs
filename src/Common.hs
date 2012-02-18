{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Common.DateTime hiding (Time)
import Control.Applicative
import Data.Maybe
import Data.Ratio hiding ((%))
import TypeLevel.List hiding (length)
import TypeLevel.Naturals
import Prelude hiding ((!!), reverse)
import Range hiding (mod, start, end)
import Utils
type YMD = (Year, Month, Day)

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
data Common n = Common
    { months     :: Year -> Range
    , days       :: Year -> Month -> Range
    , timeSplits :: YMD -> List n Integer
    , start      :: Rational
    }


class (Reduce v a, Num a) => Multiplicands a v | v -> a where
    multiplicands :: v -> v
instance (Num a) => Multiplicands a (List n a) where
    multiplicands Nil = Nil
    multiplicands (_:.v) = reduce (*) 1 v :. multiplicands v


class (Integral a) => SplitUp a v | v -> a where
    splitUp :: a -> v -> v
instance (Integral a) => SplitUp a (List n a) where
    splitUp _ Nil = Nil
    splitUp n (a:.v) = mod n a :. splitUp (div n a) v


type Time n =
    ( Applicative (List n)
    , Listable (List n)
    , Reduce (List n Integer) Integer
    , Reverse (List n Integer)
    , SplitUp Integer (List n Integer)
    , Multiplicands Integer (List n Integer)
    , ParseTime (List n) Integer
    , DottedList (List n) Integer
    , Eq (List n Integer)
    , Ord (List n Integer)
    , Natural n
    )

-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: Time n => Common n -> Year -> Integer
daysInYear f y = sum $ map (size . days f y) $ elems $ months f y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: Time n => Common n -> YMD -> Integer
timeUnitsPerDay f = reduce (*) 1 . timeSplits f

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dateOfYear :: Time n => Common n -> Year -> Integer -> YMD
dateOfYear f y n | n < 0 = dateOfYear f (y-1) (n + daysInYear f (y-1))
                 | n >= daysInYear f y = dateOfYear f (y+1) (n - daysInYear f y)
                 | otherwise = dayEnum !! n where
    dayEnum = [(y, m, d) | m <- elems $ months f y, d <- elems $ days f y m]

-- | Given YMD and the 'time' portion of a Date, determine how
-- | to split up the time portion
-- TODO: Probably needs reversing?
timeOfDay :: Time n => Common n -> YMD -> Integer -> List n Integer
timeOfDay f ymd t = reverse $ splitUp t $ reverse $ timeSplits f ymd where

-- | The year, month, and day of a date rational
largePart :: Time n => Common n -> Rational -> YMD
largePart f r | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (daysInYear f) ys)
    cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
    containing = head . filter ((abs d <) . snd)
    from (y, t) = dateOfYear f y n where n = (d-t) + daysInYear f y
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: Time n => Common n -> Rational -> List n Integer
smallPart f r = timeOfDay f (largePart f r) (timePart f r) where

-- | Just the part of the rational relevant to the day
dayPart :: Time n => Common n -> Rational -> Rational
dayPart f r = leftover r * toRational (timeUnitsPerDay f $ largePart f r)

-- | Just the part of the rational relevant to the time
timePart :: Time n => Common n -> Rational -> Integer
timePart f = floor . dayPart f

-- | Any part of the rational so small that it's not relevant to the time
detailPart :: Time n => Common n -> Rational -> Detail
detailPart f = leftover . dayPart f

-- | Split a rational into its component parts
breakDown :: Time n => Common n -> Rational -> DateTime n
breakDown f r = DateTime y m d (smallPart f r) (detailPart f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: Time n => Common n -> DateTime n -> Rational
rebuild f (DateTime y m d t x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x % timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: Time n => Common n -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: Time n => Common n -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: Time n => Common n -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: Time n => Common n -> YMD -> List n Integer -> Rational
dayFraction f ymd ts = timeInSeconds % timeUnitsPerDay f ymd where
    timeInSeconds = reduce (+) 1 $ (*) <$> ts <*> ss
    ss = multiplicands $ timeSplits f ymd

instance Time n => Calendar (Common n) where
    data Delta (Common n) = Delta
        (Maybe Year)
        (Maybe Month)
        (Maybe Day)
        (List n (Maybe Integer))
        (Maybe Detail)

    -- TODO:
    display _ _ = []
    parse _ _ = []
    plus = change madd
    minus = change msub
    clobber = change fromMaybe

    beginning = start

-- TODO
instance Read (Common v) where
    readsPrec _ _ = []
instance Show (Common v) where
    show _ = []
instance Read (Delta (Common n)) where
    readsPrec _ _ = []
instance Show (Delta (Common n)) where
    show _ = []

type Operation = Integer -> Maybe Integer -> Integer

madd, msub :: Operation
madd i = maybe i (i +)
msub i = maybe i (i -)

operation :: Time n => Operation -> DateTime n -> Delta (Common n) -> DateTime n
operation op (DateTime y m d ts x) (Delta my mm md mt mx) = DateTime
    (op y my) (op m mm) (op d md)
    (op <$> ts <*> mt)
    (op (numerator x) (numerator <$> mx) % op (denominator x) (denominator <$> mx))

change :: Time n => Operation -> Common n -> Rational -> Delta (Common n) -> Rational
change op c r = rebuild c . operation op (breakDown c r)
