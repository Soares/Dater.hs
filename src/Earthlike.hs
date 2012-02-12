module Earthlike
    ( EarthlikeFormat(..)
    , Year
    , Month
    , Day
    , YMD
    , Detail
    , breakDown
    , rebuild
    ) where
import Prelude hiding ((!!))
import Range
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
-- | You need only supply an EarthlikeFormat and this module will do the rest.
data EarthlikeFormat
    = Format
    { months :: Year -> Range
    , days :: Year -> Month -> Range
    , timeSplits :: YMD -> Time
    }


-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: EarthlikeFormat -> Year -> Integer
daysInYear f y = sum $ map (size . days f y) $ elems $ months f y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: EarthlikeFormat -> YMD -> Integer
timeUnitsPerDay f = product . timeSplits f

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dateOfYear :: EarthlikeFormat -> Year -> Integer -> YMD
dateOfYear f y n | n < 0 = dateOfYear f (y-1) (n + daysInYear f (y-1))
                 | n >= daysInYear f y = dateOfYear f (y+1) (n - daysInYear f y)
                 | otherwise = dayEnum !! n where
    dayEnum = [(y, m, d) | m <- elems $ months f y, d <- elems $ days f y m]

-- | Given YMD and the 'time' portion of a Date, determine how
-- | to split up the time portion
timeOfDay :: EarthlikeFormat -> YMD -> Integer -> Time
timeOfDay f ymd t = reverse $ splitAlong t $ reverse $ timeSplits f ymd where
    splitAlong 0 [] = []
    splitAlong n (x:xs) = mod n x : splitAlong (div n x) xs
    splitAlong _ [] = error "timeOfDay was given too much time for one day"

-- | The year, month, and day of a date rational
largePart :: EarthlikeFormat -> Rational -> YMD
largePart f r | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (daysInYear f) ys)
    cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
    containing = head . filter ((abs d <) . snd)
    from (y, t) = dateOfYear f y n where n = (d-t) + daysInYear f y
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: EarthlikeFormat -> Rational -> Time
smallPart f r = timeOfDay f (largePart f r) (timePart f r) where

-- | Just the part of the rational relevant to the day
dayPart :: EarthlikeFormat -> Rational -> Rational
dayPart f r = leftover r * toRational (timeUnitsPerDay f $ largePart f r)

-- | Just the part of the rational relevant to the time
timePart :: EarthlikeFormat -> Rational -> Integer
timePart f = floor . dayPart f

-- | Any part of the rational so small that it's not relevant to the time
detail :: EarthlikeFormat -> Rational -> Detail
detail f = leftover . dayPart f

-- | Split a rational into its component parts
breakDown :: EarthlikeFormat -> Rational -> (Year, Month, Day, Time, Detail)
breakDown f r = (y, m, d, smallPart f r, detail f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: EarthlikeFormat -> (Year, Month, Day, Time, Detail) -> Rational
rebuild f (y, m, d, t, x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x `ovr` timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: EarthlikeFormat -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: EarthlikeFormat -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: EarthlikeFormat -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: EarthlikeFormat -> YMD -> Time -> Rational
dayFraction f ymd ts = timeInSeconds `ovr` (head mods) where
    timeInSeconds = sum $ zipWith (*) sections (tail mods)
    sections = pad 0 numSections $ take numSections ts
    numSections = length $ timeSplits f ymd
    mods = prods $ timeSplits f ymd
