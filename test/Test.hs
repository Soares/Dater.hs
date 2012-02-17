module Test where
import Control.Category ((.))
import Data.Label
import Prelude hiding ((.), id)
import Relative
import Date hiding (add, sub)
import Era
import Earth
import Earthlike

-- The gregorian era begins at year 1, which is 366 days from year 0
-- (because year 0 would have been a leap year, had it existed)
-- (0 counts so we use 365)
g :: Era
g = Era "Gregorian" gregorian 365 [] []

-- A whole bunch of days (not exactly 220 years),
-- 3 hours, and one half of a second
t :: Date
t = Date g  $ (2019 * 365) + (3 / 24) + (1 / (24*60*60*2))

-- A year, month, day, hour, minute, and second
r :: Relative
r = [Just 1, Just 1, Just 1, Just 1, Just 1, Just 1]

display dt = breakDown (get (format . era) dt) (get value dt)

main :: IO ()
main = do
    print $ display t
    print $ display $ add t r
