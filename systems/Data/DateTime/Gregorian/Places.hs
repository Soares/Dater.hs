module Data.DateTime.Gregorian.Places where

data Place
    = Africa
    | Antarctica
    | Asia
    | Atlantic
    | Australia
    | Caribbean
    | CentralAmerica
    | Everywhere
    | Europe
    | IndianOcean
    | Military
    | NorthAmerica
    | Pacific
    | SouthAmerica
    deriving (Eq, Ord, Read, Show, Enum)
