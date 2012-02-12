module LaahC where
import Earthlike

ak :: EarthlikeFormat
ak = Format ms ds ts where
    ms = const (0, 12) -- Includes zero-month

    ds y m d
        | m == 0 && isLeapYear y = (0, 1)
        | m == 0 = (0, 0)
        | m `elem` [2, 5, 8, 11] = (1, 31)
        | otherwise = (1, 30)

    isLeapYear y
        | y `mod` 400 == 0 = True
        | y `mod` 100 == 0 = False
        | y `mod` 4 == 0 = True
        | otherwise = False
