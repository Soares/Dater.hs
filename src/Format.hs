module Format where
import Control.Arrow

-- TODO: out of date
-- Formatters will be like
--  Left (0, Number)
--      Left = super day, Right = sub day
--      number = component it applies to
-- data Style = Number | Name   | Abbreviation Int
-- year  2012 → 2012   | 20     | [12]
-- month 3    → 3      | March  | [Mar]
-- day   33   → 2      | Monday | [Mon, Mo, M]
-- (Note: would it be day 33 or day 2?)

data Style = Number | Name | Abbreviation Int
type Formatter = (Int, Style)

class Format a where
    display :: Formatter -> a -> String

level :: Formatter -> Int
level = fst

style :: Formatter -> Style
style = snd

descend :: Formatter -> Formatter
descend = first $ (-) 1
