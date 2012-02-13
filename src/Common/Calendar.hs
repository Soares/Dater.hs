{-# LANGUAGE TypeFamilies #-}
module Common.Calendar ( ) where
import Calendar
import Common.Date
import Control.Category (id)
import Data.Ratio hiding ((%))
import Prelude hiding ((.), (!!), id)
import Utils

type Op = Integer -> Maybe Integer -> Integer
type Unwrapped = (Year, Month, Day, Time, Detail)
type Operation = Unwrapped -> [Maybe Integer] -> Unwrapped

instance Calendar Date where
    -- TODO: Parameterize the number of elements in Time
    data Delta Date = Delta [Maybe Integer]

    readDelta _ = []        -- TODO
    showDelta _ = const ""  -- TODO
    display _ _ = []        -- TODO

    plus = change madd
    minus = change msub
    clobber = change mright

    normalize _ _ = 0   -- TODO
    denormalize _ _ = 0 -- TODO

madd, msub, mright :: Op
madd i = maybe i (i +)
msub i = maybe i (i -)
mright i = maybe i id

operation :: Op -> Operation
operation op (y, m, d, ts, x) mns = (y', m', d', ts', x') where
    at n = if toInteger (length mns) > n then mns !! n else Nothing
    y' = op y (at 0)
    m' = op m (at 1)
    d' = op d (at 2)
    dot t n = op t (at n)
    ts' = zipWith dot ts [3..]
    ni = toInteger $ 3 + length ts
    top = op (numerator x) (at ni)
    bot = op (denominator x) (at $ ni + 1)
    x' = top % bot

change :: Op -> Date -> Rational -> Delta Date -> Rational
change op d rat (Delta rel) = rebuild d $ operation op (breakDown d rat) rel

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake f rel = rebuild f (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}
