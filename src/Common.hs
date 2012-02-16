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
import Control.Applicative
import Data.Maybe
import Data.Ratio hiding ((%))
import Data.Vec ( Reverse', Fold, Map, (:.)((:.)) )
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
type Year = Integer
-- | A primary unit, dependent upon the secondary unit
type Month = Integer
-- | A secondary unit
type Day = Integer
-- | (Year, Month, and Day can be abbreviated YMD)
type YMD = (Year, Month, Day)
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
data Common v = Cal
    { months        :: Year -> Range
    , days          :: Year -> Month -> Range
    , timeSplits    :: NList v Integer => YMD -> v Integer
    , beginning     :: Rational
    }

data DateTime v = DateTime
    { year   :: Year
    , month  :: Month
    , day    :: Day
    , time   :: NList v Integer => v Integer
    , detail :: Detail
    }

class Maybeify v v' | v -> v' where
    maybeify :: v -> v'
instance Maybeify () () where
    maybeify () = ()
instance (Maybeify v v') => Maybeify (a :. v) (Maybe a :. v') where
    maybeify (a :. v) = (Just a :. maybeify v)

class Combineable a b c u v w
    | u -> a
    , v -> b
    , w -> c
    , a v w -> u
    , b u w -> v
    , c v u -> w
    where
    combine :: (a -> b -> c) -> u -> v -> w
instance Combineable a b c (a:.()) (b:.()) (c:.()) where
    combine f (a :. ()) (b :. ()) = f a b :. ()
instance (Combineable a b c (a:.u) (b:.v) (c:.w)) => Combineable a b c (a:.a:.u) (b:.b:.v) (c:.c:.w) where
    combine f (a :. u) (b :. v) = f a b :. combine f u v

class (Fold v a, Num a) => Multiplicands a v | v -> a where
    multiplicands :: v -> v
instance (Num a) => Multiplicands a (a :. ()) where
    multiplicands (_ :. ()) = 1 :. ()
instance (Num a, Fold (a :. v) a, Multiplicands a (a :. v)) => Multiplicands a (a :. a :. v) where
    multiplicands (_ :. as) = Vec.product as :. multiplicands as

class (Integral a) => SplitUp a v where
    splitUp :: a -> v -> v
instance (Integral a) => SplitUp a () where
    splitUp _ () = ()
instance (Integral a, SplitUp a v) => SplitUp a (a :. v) where
    splitUp n (x :. xs) = mod n x :. splitUp (div n x) xs

data Vec n a where
    None :: Vec N0 a
    Next :: a -> Vec n a -> Vec (Succ n) a
type NList v i =
    ( Fold (v i) i
    , SplitUp i (v i)
    , Reverse' () (v i) (v i)
    , Map i i (v i) (v i)
    , Multiplicands i (v i)
    , Combineable i i i (v i) (v i) (v i)
    , Combineable i (Maybe i) i (v i) (v (Maybe i)) (v i)
    )

-- | The number of days in a year.
-- | Defaults to counting the number of days in all months that year.
daysInYear :: NList v Integer => Common v -> Year -> Integer
daysInYear f y = sum $ map (size . days f y) $ elems $ months f y

-- | The number of 'seconds' in a day.
timeUnitsPerDay :: NList v Integer => Common v -> YMD -> Integer
timeUnitsPerDay f = Vec.product . timeSplits f

-- | Given a year and the 'day' portion of a Date, determine the Month/Day
-- | pair of the date.
dateOfYear :: NList v Integer => Common v -> Year -> Integer -> YMD
dateOfYear f y n | n < 0 = dateOfYear f (y-1) (n + daysInYear f (y-1))
                 | n >= daysInYear f y = dateOfYear f (y+1) (n - daysInYear f y)
                 | otherwise = dayEnum !! n where
    dayEnum = [(y, m, d) | m <- elems $ months f y, d <- elems $ days f y m]

-- | Given YMD and the 'time' portion of a Date, determine how
-- | to split up the time portion
-- TODO: Probably needs reversing?
timeOfDay :: NList v Integer => Common v -> YMD -> Integer -> v Integer
timeOfDay f ymd t = Vec.reverse $ splitUp t $ Vec.reverse $ timeSplits f ymd where

-- | The year, month, and day of a date rational
largePart :: NList v Integer => Common v -> Rational -> YMD
largePart f r | d < 0 = from . containing $ dayedYears [-1,-2..]
              | otherwise = from . containing $ dayedYears [0..] where
    dayedYears ys = zip ys (cascade $ map (daysInYear f) ys)
    cascade xs = x : map (x+) (cascade $ tail xs) where x = head xs
    containing = head . filter ((abs d <) . snd)
    from (y, t) = dateOfYear f y n where n = (d-t) + daysInYear f y
    d = floor r

-- | All the chunks of a day (i.e. Hour, Minute, Second, etc.)
smallPart :: NList v Integer => Common v -> Rational -> v Integer
smallPart f r = timeOfDay f (largePart f r) (timePart f r) where

-- | Just the part of the rational relevant to the day
dayPart :: NList v Integer => Common v -> Rational -> Rational
dayPart f r = leftover r * toRational (timeUnitsPerDay f $ largePart f r)

-- | Just the part of the rational relevant to the time
timePart :: NList v Integer => Common v -> Rational -> Integer
timePart f = floor . dayPart f

-- | Any part of the rational so small that it's not relevant to the time
detailPart :: NList v Integer => Common v -> Rational -> Detail
detailPart f = leftover . dayPart f

-- | Split a rational into its component parts
breakDown :: NList v Integer => Common v -> Rational -> DateTime v
breakDown f r = DateTime y m d (smallPart f r) (detailPart f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: NList v Integer => Common v -> DateTime v -> Rational
rebuild f (DateTime y m d t x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x % timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: NList v Integer => Common v -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: NList v Integer => Common v -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: NList v Integer => Common v -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: NList v Integer => Common v -> YMD -> v Integer -> Rational
dayFraction f ymd ts = timeInSeconds % timeUnitsPerDay f ymd where
    timeInSeconds = Vec.sum $ combine (*) ts (multiplicands $ timeSplits f ymd)

instance NList v Integer => Calendar (Common v) where
    data Delta (Common v) = Delta
        { dYear   :: Maybe Year
        , dMonth  :: Maybe Month
        , dDay    :: Maybe Day
        , dTime   :: v (Maybe Integer)
        , dDetail :: Maybe Detail
        }

    -- TODO:
    display _ _ = []
    parse _ _ = []
    plus = change madd
    minus = change msub
    clobber = change fromMaybe

    normalize c r = r + beginning c
    denormalize c r = r - beginning c

-- TODO
instance Read (Common v) where
    readsPrec _ _ = []
instance Show (Common v) where
    show _ = []
instance Read (Delta (Common v)) where
    readsPrec _ _ = []
instance Show (Delta (Common v)) where
    show _ = []

type Operation = Integer -> Maybe Integer -> Integer

madd, msub :: Operation
madd i = maybe i (i +)
msub i = maybe i (i -)

operation :: NList v Integer => Operation -> DateTime v -> Delta (Common v) -> DateTime v
operation op (DateTime y m d ts x) (Delta my mm md mt mx) = DateTime
    (op y my) (op m mm) (op d md)
    (combine op ts mt)
    (op (numerator x) (numerator <$> mx) % op (denominator x) (denominator <$> mx))

change :: NList v Integer => Operation -> Common v -> Rational -> Delta (Common v) -> Rational
change op c r = rebuild c . operation op (breakDown c r)

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake c rel = rebuild c (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}
