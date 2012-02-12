module Earthlike where

leftover :: Rational -> Rational
leftover r = r - toRational (floor r :: Integer)

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
-- | A series of inner units
type Chunk = Integer
type Time = [Chunk]
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
    { months :: Year -> [(Month, [Day])]
    , chunks :: Year -> Month -> Day -> [Chunk]
    }


-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: Year -> EarthlikeFormat -> Integer
daysInYear y = toInteger . length . concatMap snd . flip months y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: Year -> Month -> Day -> EarthlikeFormat -> Integer
timeUnitsPerDay y m d f = product $ chunks f y m d

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dayOfYear :: Year -> Integer -> EarthlikeFormat -> (Month, Day)
dayOfYear y n f | n < 0 = dayOfYear (y-1) (n+daysInYear (y-1) f) f
                | n > daysInYear y f = dayOfYear (y+1) (n-daysInYear y f) f
                | otherwise = l2Find (months f y) n where
    l2Find ((month, day:_):_) 0 = (month, day)
    l2Find ((month, _:days):rest) x = l2Find ((month, days):rest) (x-1)
    l2Find ((_, []):rest) x = l2Find rest x
    -- This state should not be reachable.
    l2Find [] _ = undefined

-- | Given a year & month & the 'time' portion of a Date, determine how
-- | to split up teh time portion
timeOfDay :: Year -> Month -> Day -> Integer -> EarthlikeFormat -> Time
timeOfDay y m d t f = reverse $ splitAlong (reverse $ chunks f y m d) t where
    splitAlong [] 0 = []
    splitAlong (x:xs) n = mod n x:splitAlong xs (div n x)
    -- This state should not be reachable.
    splitAlong [] _ = undefined

-- | The year, month, and day of a date rational
largePart :: Rational -> EarthlikeFormat -> (Year, Month, Day)
largePart r f | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (`daysInYear` f) ys)
    cascade (x:xs) = x:map (x+) (cascade xs)
    cascade [] = []
    containing = head . filter ((abs d <) . snd)
    from (i, t) = (i, month, day) where
        (month, day) = dayOfYear i n f
        n = (d - t) + daysInYear i f
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: Rational -> EarthlikeFormat -> [Integer]
smallPart r f = timeOfDay y m d i f where
    (y, m, d) = largePart r f
    i = timePart r f

dayPart :: Rational -> EarthlikeFormat -> Rational
dayPart r f = leftover r * toRational (timeUnitsPerDay y m d f)
    where (y, m, d) = largePart r f

timePart :: Rational -> EarthlikeFormat -> Integer
timePart r = floor . dayPart r

detail :: Rational -> EarthlikeFormat -> Detail
detail r = leftover . dayPart r
