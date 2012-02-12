module Earth where
import Earthlike

gregorian :: EarthlikeFormat
gregorian = Format ms ds ts where
    ms = const (1, 12)

    ds y d
        | d == 2 && isLeapYear y = (1, 29)
        | d == 2 = (1, 28)
        | d `elem` [9, 4, 6, 11] = (1, 30)
        | otherwise = (1, 31)

    ts = const [24, 60, 60]

    isLeapYear y
        | y `mod` 400 == 0 = True
        | y `mod` 100 == 0 = False
        | y `mod` 4 == 0 = True
        | otherwise = False
