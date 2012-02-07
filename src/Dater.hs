module Dater where

type Date = (Integer, Integer)

data (Date m d) => DateFormat m d = DateFormat
    { name          :: String
    , codes         :: [String]
    , precodes      :: [String]
    , moment        :: m
    , delta         :: d
    , parseDelta    :: ReadS d
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
