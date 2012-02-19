{-# LANGUAGE TemplateHaskell #-}
module Earth where
import Language.Haskell.TH
import Vector
import Common

data Year = Common | Leap deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Common.Year Year where
    typeFrom y
        | y `mod` 400 == 0 = Leap
        | y `mod` 100 == 0 = Common
        | y `mod` 4 == 0 = Leap
        | otherwise = Common


data Month =
    January | February  | March     |
    April   | May       | June      |
    July    | August    | September |
    October | November  | December
    deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Common.Month Month where
    firstDay _ _ = 1
    daysIn Common February = 28
    daysIn Leap February = 29
    daysIn _ m | m `elem` [September, April, June, November] = 30
               | otherwise = 31


data Day =
    Monday  | Tuesday    | Wednesday | Thursday |
    Friday  | Saturday   | Sunday
    deriving (Eq, Ord, Read, Show, Bounded, Enum)
instance Common.Day Days where
    startsOn = Sunday


newtype Hour = H $(zMod 24) deriving (Eq, Ord, Read, Enum, Bounded, Num)
instance Show Hour where show (H h) | h < 1 || h > 12 = show $ n $ h - 12
instance Common.TimePart Hour


newtype Minute = M $(zMod 60) deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)
instance Common.TimePart Minute


newtype Second = S $(zMod 60) deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)
instance Common.TimePart Second
    

instance Common (Year |/| Month |/| Day) (Hour |:| Minute |:| Second)
