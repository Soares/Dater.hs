module Text.Format.DateTime.Standard where

data Standard
    = DateTime
    | Date
    | Time
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

class Format a where
    table :: [(Char, Either (a, Style) String)]

instance Format Standard where
    table =
        -- TODO: Parse immediately instead of leaving strings around
        [ ('a', Left (WeekDay, Abbreviation 3))
        , ('A', Left (WeekDay, Name))
        , ('b', Left (Month, Abbreviation 3))
        , ('B', Left (Month, Name))
        , ('c', Right "%a %b %-d, %T %Y")
        , ('C', Left (Century, Number))
        , ('d', Left (MonthDay, Number))
        , ('D', Right "%d/%m/%Y")
        , ('e', Right "%_d")
        , ('f', Right "%Y-%m-%d")
        , ('F', Left (Fraction, Name))
        , ('g', Right "%~y")
        , ('G', Right "%~Y")
        , ('h', Right "%b")
        , ('H', Left (Hour, Number))
        , ('i', Left (Hour, Abbreviation 3))
        , ('I', Left (Hour, Name))
        , ('j', Left (Date, Number))
        , ('m', Left (Month, Number))
        , ('k', Right "%_H")
        , ('K', Right "%_I")
        , ('M', Left (Minute, Number))
        , ('N', Left (Fraction, Number))
        , ('p', Left (Meridian, Name))
        , ('P', Right "%^p")
        , ('r', Right "%H:%M:%S.%N")
        , ('R', Right "%H:%M:%S.%F")
        , ('s', Left (DateTime, Number))
        , ('S', Left (Time, Number))
        , ('T', Right "%H:%M:%S")
        , ('u', Right "%:w")
        , ('U', Right "%:W")
        , ('V', Right "%~W")
        , ('w', Left (WeekDay, Number))
        , ('W', Left (Week, Number))
        , ('X', Right "%H:%M")
        , ('y', Left (Year, Abbreviation 2))
        , ('Y', Left (Year, Number))
        , ('z', Left (TimeZone, Number))
        , ('Z', Left (TimeZone, Name))
        ]
