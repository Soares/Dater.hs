module Text.Format.Test where
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
        [ ('a', Directive WeekDay (Abbreviation 3))
        , ('A', Directive WeekDay Name)
        , ('b', Directive Month (Abbreviation 3))
        , ('B', Directive Month Name)
        , ('c', shortcut "%a %b %-d, %T %Y")
        , ('C', Directive Century Number)
        , ('d', Directive MonthDay Number)
        , ('D', shortcut "%d/%m/%Y")
        , ('e', shortcut "%_d")
        , ('f', shortcut "%Y-%m-%d")
        , ('F', Directive Fraction Name)
        , ('g', shortcut "%~y")
        , ('G', shortcut "%~Y")
        , ('h', shortcut "%b")
        , ('H', Directive Hour Number)
        , ('i', Directive Hour (Abbreviation 3))
        , ('I', Directive Hour Name)
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
        , ('s', Directive DateTime Number)
        , ('S', Directive Time Number)
        , ('T', shortcut "%H:%M:%S")
        , ('u', shortcut "%:w")
        , ('U', shortcut "%:W")
        , ('V', shortcut "%~W")
        , ('w', Directive WeekDay Number)
        , ('W', Directive Week Number)
        , ('X', shortcut "%H:%M")
        , ('y', Directive Year (Abbreviation 2))
        , ('Y', Directive Year Number)
        , ('z', Directive TimeZone Number)
        , ('Z', Directive TimeZone Name)
        ]
