module Common.DateTime where

data DateTime v = DateTime
    Year
    Month
    Day
    (NList v Integer => v Integer)
    Detail

-- TODO: Read/Show
