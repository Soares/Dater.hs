module Text.Format.Calendar.Standard where
import Text.Format.Table
import Text.Format.Parse
import qualified Data.Map as Map

data Standard
    = DateTime
    | Date
    | Time
    | TimeZone
    | Century
    | Year
    | Month
    | MonthDay
    | Week
    | WeekDay
    | Hour
    | Meridium
    | Minute
    | Second
    | Fraction
    deriving (Eq, Ord, Read, Show)

instance Format Standard where
    table = Map.fromList
        [ ('a', shortcut "%:A")
        , ('A', Directive WeekDay Name)
        , ('b', shortcut "%:B")
        , ('B', Directive Month Name)
        , ('c', shortcut "%a %b %-d, %T %Y")
        , ('C', Directive Century Number)
        , ('d', Directive MonthDay Number)
        , ('D', shortcut "%d/%m/%Y")
        , ('e', shortcut "%_d")
        , ('E', Directive DateTime Number)
        , ('f', shortcut "%Y-%m-%d")
        , ('F', Directive Fraction Name)
        , ('g', shortcut "%~y")
        , ('G', shortcut "%~Y")
        , ('h', shortcut "%b")
        , ('H', Directive Hour Number)
        , ('I', shortcut "%:H")
        , ('j', Directive Date Number)
        , ('m', Directive Month Number)
        , ('k', shortcut "%_H")
        , ('K', shortcut "%_I")
        , ('M', Directive Minute Number)
        , ('N', Directive Fraction Number)
        , ('p', Directive Meridium Name)
        , ('P', shortcut "%^p")
        , ('r', shortcut "%H:%M:%S.%N")
        , ('R', shortcut "%H:%M:%S.%F")
        , ('s', Directive Time Number)
        , ('S', Directive Second Number)
        , ('T', shortcut "%H:%M:%S")
        , ('u', shortcut "%:w")
        , ('U', shortcut "%:W")
        , ('V', shortcut "%~W")
        , ('w', Directive WeekDay Number)
        , ('W', Directive Week Number)
        , ('X', shortcut "%H:%M")
        , ('y', shortcut "%:Y")
        , ('Y', Directive Year Number)
        , ('z', Directive TimeZone Number)
        , ('Z', Directive TimeZone Name)
        ]