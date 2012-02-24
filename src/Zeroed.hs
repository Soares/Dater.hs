{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Zeroed where
-- Why not just use 'Num'? Well, a lot of dates don't start on
-- '0'. For example, the gregorian calendar begins on Year 1.
--
-- I'm not convinced that this is a good abstraction yet.
-- It seems to overlap with Range(start).
--
-- However:
--
-- * There are date systems that are sometimes zero-indexed and sometimes
--   one-indexed. (i.e. calendars where the 'leap year' is a zero-month
--   with a zero-day, but otherwise months/days start with 1)
-- * We don't want to have a 'Range' requirement on the leftmost element
--   of a VarPart (what would the context be?)
--
-- Which seems to justify the separation between Range(start) and Zeroed.
--
-- The alternative here would be to 'internally' use 0 as 'Year 1' and make
-- users override "Show Year" to show one plus the year.
-- I'm on the fence right now.

class Zeroed a where zero :: a
instance Zeroed Integer where zero = 0
instance Zeroed Rational where zero = 0
instance Zeroed Int where zero = 0
