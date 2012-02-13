{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common
    ( Common(..)
    , Year
    , Month
    , Day
    , YMD
    , Time
    , Detail
    , breakDown
    , rebuild
    ) where
import Calendar
import Data.Ratio hiding ((%))
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
type Year = Integer
-- | A primary unit, dependent upon the secondary unit
type Month = Integer
-- | A secondary unit
type Day = Integer
-- | (Year, Month, and Day can be abbreviated YMD)
type YMD = (Year, Month, Day)
-- | A series of inner units
type Time = [Integer]
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
data Common = Cal
    { months        :: Year -> Range
    , days          :: Year -> Month -> Range
    , timeSplits    :: YMD -> Time
    , beginning     :: Rational
    }


-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: Common -> Year -> Integer
daysInYear f y = sum $ map (size . days f y) $ elems $ months f y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: Common -> YMD -> Integer
timeUnitsPerDay f = product . timeSplits f

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dateOfYear :: Common -> Year -> Integer -> YMD
dateOfYear f y n | n < 0 = dateOfYear f (y-1) (n + daysInYear f (y-1))
                 | n >= daysInYear f y = dateOfYear f (y+1) (n - daysInYear f y)
                 | otherwise = dayEnum !! n where
    dayEnum = [(y, m, d) | m <- elems $ months f y, d <- elems $ days f y m]

-- | Given YMD and the 'time' portion of a Date, determine how
-- | to split up the time portion
timeOfDay :: Common -> YMD -> Integer -> Time
timeOfDay f ymd t = reverse $ splitAlong t $ reverse $ timeSplits f ymd where
    splitAlong 0 [] = []
    splitAlong n (x:xs) = mod n x : splitAlong (div n x) xs
    splitAlong _ [] = error "timeOfDay was given too much time for one day"

-- | The year, month, and day of a date rational
largePart :: Common -> Rational -> YMD
largePart f r | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (daysInYear f) ys)
    cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
    containing = head . filter ((abs d <) . snd)
    from (y, t) = dateOfYear f y n where n = (d-t) + daysInYear f y
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: Common -> Rational -> Time
smallPart f r = timeOfDay f (largePart f r) (timePart f r) where

-- | Just the part of the rational relevant to the day
dayPart :: Common -> Rational -> Rational
dayPart f r = leftover r * toRational (timeUnitsPerDay f $ largePart f r)

-- | Just the part of the rational relevant to the time
timePart :: Common -> Rational -> Integer
timePart f = floor . dayPart f

-- | Any part of the rational so small that it's not relevant to the time
detail :: Common -> Rational -> Detail
detail f = leftover . dayPart f

-- | Split a rational into its component parts
breakDown :: Common -> Rational -> Unwrapped
breakDown f r = (y, m, d, smallPart f r, detail f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: Common -> Unwrapped -> Rational
rebuild f (y, m, d, t, x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x % timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: Common -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: Common -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: Common -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: Common -> YMD -> Time -> Rational
dayFraction f ymd ts = timeInSeconds % head mods where
    timeInSeconds = sum $ zipWith (*) sections (tail mods)
    sections = pad 0 numSections $ take numSections ts
    numSections = length $ timeSplits f ymd
    mods = prods $ timeSplits f ymd



instance Calendar Common where
    -- TODO: Parameterize the number of elements in Time
    data Delta Common = Delta [Maybe Integer]

    display _ _ = []
    parse _ _ = []

    plus = change madd
    minus = change msub
    clobber = change mright

    normalize c r = r + beginning c
    denormalize c r = r - beginning c

-- TODO
instance Read Common where
    readsPrec _ _ = []
instance Show Common where
    show _ = []
instance Read (Delta Common) where
    readsPrec _ _ = []
instance Show (Delta Common) where
    show _ = []

type Operation = Integer -> Maybe Integer -> Integer
type Unwrapped = (Year, Month, Day, Time, Detail)

madd, msub, mright :: Operation
madd i = maybe i (i +)
msub i = maybe i (i -)
mright i = maybe i id

operation :: Operation -> Unwrapped -> Delta Common -> Unwrapped
operation op (y, m, d, ts, x) (Delta mns) = (y', m', d', ts', x') where
    at n = if toInteger (length mns) > n then mns !! n else Nothing
    y' = op y (at 0)
    m' = op m (at 1)
    d' = op d (at 2)
    dot t n = op t (at n)
    ts' = zipWith dot ts [3..]
    ni = toInteger $ 3 + length ts
    top = op (numerator x) (at ni)
    bot = op (denominator x) (at $ ni + 1)
    x' = top % bot

change :: Operation -> Common -> Rational -> Delta Common -> Rational
change op d rat = rebuild d . operation op (breakDown d rat)

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake f rel = rebuild f (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}
