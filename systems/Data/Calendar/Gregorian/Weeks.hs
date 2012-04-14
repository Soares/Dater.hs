{-# LANGUAGE TypeOperators #-}
module Data.Calendar.Gregorian.Weeks where
import Control.Applicative
import Data.Calendar.Gregorian.Date
import Data.Modable
import Text.Format.Write

data WeekDay
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance WriteBlock WeekDay where
    textual = pure . show
    numerical = pure . show . fromEnum

seven :: Integral a => a
seven = fromIntegral . length $ [minBound .. maxBound :: WeekDay]

next :: WeekDay -> WeekDay
next w = if w == Saturday then Sunday else succ w

prev :: WeekDay -> WeekDay
prev w = if w == Sunday then Saturday else pred w

abbrevs :: WeekDay -> [String]
abbrevs Tuesday = ["Tue", "Tues"]
abbrevs Wednesday = ["Wed", "Wednes"]
abbrevs Thursday = ["Thu", "Thur", "Thurs"]
abbrevs w = pure $ take 3 $ show w

names :: WeekDay -> [String]
names w = [show w, take 3 $ show w] ++ abbrevs w

weekDay :: Date -> WeekDay
weekDay d = toEnum $ (o + fromIntegral d) `mod` seven where o = 0

-- The week year begins on the week that contains Jan 4
startOfWeekYear :: Date -> Date
-- TODO: abstract out contsruction/destruction
startOfWeekYear d = startOfMonWeek (year d /- 1 /- 4)

daysSinceBeginningOfWeekYear :: Date -> Integer
daysSinceBeginningOfWeekYear d = d `sub` startOfWeekYear d

weekYear :: Date -> Year
weekYear d = (if delta < 0 then pred else id) (year d)
    where delta = daysSinceBeginningOfWeekYear d

firstSunInYear :: Date -> Date
firstSunInYear d = startOfSunWeek (year d /- 1 /- 7)

firstMonInYear :: Date -> Date
firstMonInYear d = startOfMonWeek (year d /- 1 /- 7)

startOfSunWeek :: Date -> Date
startOfSunWeek d = d `less` toInteger n where
    n = fromEnum $ weekDay d

startOfMonWeek :: Date -> Date
startOfMonWeek d = d `less` toInteger n where
    n = pred (fromEnum $ weekDay d) `mod` seven

numOfWeekMonFirst :: WeekDay -> Int
numOfWeekMonFirst Sunday = seven
numOfWeekMonFirst w = fromEnum w

numOfWeekSunFirst :: WeekDay -> Int
numOfWeekSunFirst = fromEnum

-- [0..53]
weekInYearMonFirst :: Date -> Int
weekInYearMonFirst d = (fromInteger delta) `div` seven
    where delta = d `sub` firstMonInYear d

-- [0..53]
weekInYearSunFirst :: Date -> Int
weekInYearSunFirst d = (fromInteger delta) `div` seven
    where delta = d `sub` firstSunInYear d

-- [1..53]
weekInWeekYear :: Date -> Int
weekInWeekYear d = succ $ weekInYearMonFirst d


-- TODO: Modable is ALL WRONG
minus :: Modable a => a -> a -> Relative a
minus = undefined

sub :: Modable a => a -> a -> Integer
sub = undefined

less :: Modable a => a -> Integer -> a
less = undefined