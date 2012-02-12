module Relative where
import Control.Category ((.), id)
import Data.Label
import Date
import Earthlike
import Era
import Prelude hiding ((.), id)

type Relative = [Maybe Integer]
type Op = Integer -> Maybe Integer -> Integer
type Operation = [Integer] -> [Maybe Integer] -> [Integer]

madd :: Op
madd i = maybe i (+ i)

msub :: Op
msub i = maybe i (- i)

mright :: Op
mright i = maybe i id

operation :: Op -> Operation
operation _ [] _ = []
operation _ xs [] = xs
operation op (x:xs) (y:ys) = op x y:change op xs ys

change :: Op -> Date -> Relative -> Date
change op dt rel = wrap (era dt) $ fromInts (operation op ints rel) f where
    f = get (format . era) d
    ints = toInts (get value d) f

add, sub, clobber :: Date -> Relative -> Date
add = change madd
sub = change msub
clobber = change mright

toInts :: Date -> [Integer]
fromInts :: Era -> [Integer] -> Date
fromInts e (y:m:d:cs) = wrap e $ rebuild f (y, m, d, t, x) where
    f = get format e
    n = length $ timeSplits f (y, m, d)
    t = take n cs
    xs = drop n cs
    x = if null extras then 0 else underOne $ read $ concatMap show xs
    underOne i = i `ovr` orderOf i
    orderOf i = read $ '1':['0' | _ <- show n]
