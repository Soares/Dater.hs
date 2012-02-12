module Relative
    ( Relative
    , add
    , sub
    , clobber
    ) where
import Control.Category ((.), id)
import Data.Label
import Data.Ratio hiding ((%))
import Date hiding (add, sub)
import Earthlike
import Era
import Prelude hiding ((.), (!!), id)
import Utils


type Relative = [Maybe Integer]
type Op = Integer -> Maybe Integer -> Integer
type Unwrapped = (Year, Month, Day, Time, Detail)
type Operation = Unwrapped -> Relative -> Unwrapped

madd :: Op
madd i = maybe i (i +)

msub :: Op
msub i = maybe i (i -)

mright :: Op
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

change :: Op -> Date -> Relative -> Date
change op dt = Date e . rebuild f . operation op (breakDown f v) where
    e = get era dt
    f = get format e
    v = get value dt

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake f rel = rebuild f (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}


add, sub, clobber :: Date -> Relative -> Date
add = change madd
sub = change msub
clobber = change mright
