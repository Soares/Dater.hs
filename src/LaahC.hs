module LaahC where
import Earthlike

ak :: EarthlikeFormat
ak = Format monthSplits timeSplits where
    timeSplits _ _ _ = [10, 100, 100]
    monthSplits y | isLeapYear y = (0, [0, 1]) : standardMonths
                  | otherwise = (0, [0]) : standardMonths
    standardMonths = [(m, [1..daysInMonth m]) | m <- [1..12]]
    daysInMonth m | m `elem` [2, 5, 8, 11] = 31
                  | otherwise = 30
    isLeapYear y | y `mod` 400 == 0 = True
                 | y `mod` 100 == 0 = False
                 | y `mod` 4 == 0 = True
                 | otherwise = False

gregorian :: EarthlikeFormat
gregorian = Format monthSplits timeSplits where
    timeSplits _ _ _ = [24, 60, 60]
    monthSplits y | isLeapYear y = leapMonths
                  | otherwise = standardMonths
    standardMonths = [(m, [1..daysInMonth m]) | m <- [1..12]]
    daysInMonth m | m == 2 = 28
                  | m `elem` [9, 4, 6, 11] = 30
                  | otherwise = 31
    leapMonths = j:(fm, oneMore fd):r where (j:(fm, fd):r) = standardMonths
    oneMore [] = []
    oneMore [x] = [x, x+1]
    oneMore (x:xs) = x : oneMore xs
    isLeapYear y | y `mod` 400 == 0 = True
                 | y `mod` 100 == 0 = False
                 | y `mod` 4 == 0 = True
                 | otherwise = False
