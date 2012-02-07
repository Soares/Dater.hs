module LaahC where
import Dater

type Year = Integer
type Month = (Int, [Day])
type Day = Int


data EarthlikeDateFormat = ELD
    { unitsInDay  :: Integer
    , monthSplits :: Year -> [Month]
    , timeSplits  = [Int]
    }

convert :: EarthlikeDateFormat -> EarthlikeDateFormat -> Rational -> Rational
convert a b x = (x / unitsInDay a) * (unitsInDay b)

daysInYear :: Year -> EarthlikeDateFormat -> Int
daysInYear y = length . concat . map snd . monthSplits y

dayOfYear :: Year -> Int -> EarthlikeDateFormat -> (Int, Int)
dayOfYear y n e | n < 0 = dayOfYear (y-1) (n+(daysInYear (y-1) e)) e
                | n > daysInYear y e = dayOfYear (y+1) (n-(daysInYear y e)) e
                | otherwise = l2Find (monthSplits y e) n where
    l2Find ((month, (day:_)):_) 0 = (month, day)
    l2Find ((month, (_:days)):rest) n = deepElem ((month, days):rest) (n-1)
    l2Find ((_, []):rest) n = deepElem rest n
    l2Find [[]] = undefined

timeOfDay :: Integer -> EarthlikeDateFormat -> [Int]
timeOfDay 


ak :: EarthlikeDateFormat
ak = ELD
    { unitsInDay  = 100000
    , monthSplits = akMonthSplits
    , timeSplits  = akTimeSplits
    }

monthSplits y | isLeapYear y = (0, [0, 1]) : standardMonths
              | otherwise = (0, [0]) : standardMonths

timeSplits = [10, 100, 100]

standardMonths = [(m, [1..daysInMonth m] | m <- [1..12])] where
    daysInMonth m | m `elem` [2, 5, 8, 11] = 31
                  | otherwise = 30

isLeapYear y | y `mod` 400 == 0 = True
             | y `mod` 100 == 0 = False
             | y `mod` 4 == 0 = True
             | otherwise = False

newtype Ak = Ak { _value :: Rational }
instance EarthlikeDate Ak where
    unitsInDay = 100000
    value = _value
    daysInYear y _ | 

    
instance (EarthlikeDate d) => Date d where
    ...









pke :: DateFormat PKEAbs PKERel
pke = DateFormat {
    name = "Post-Kaol Era"
    codes = ["PKE", "PK"]
    precodes = ["BKE", "BK"]

data (Date m d) => DateFormat = DateFormat
    { name          :: String
    , codes         :: [String]
    , precodes      :: [String]
    , relationships :: [Maybe Descendence]
    }

class Date m d where
    data (Moment m) => Absolute m
    data (Delta d) => Relative d
    add :: d -> m -> d
    sub :: d -> m -> d
    clobber :: d -> m -> d

class (Read m, Show m) => Moment m where
    absolute :: m -> (Integer, Integer)

class (Read m, Show m) => Delta d where
    relative :: m -> [Maybe Integer]
