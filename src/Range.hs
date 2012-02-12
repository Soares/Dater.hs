module Range
    ( Range
    , (%)
    , contains
    , size
    , elems
    , over
    , under
    , start
    , end
    , push
    ) where
type Range = (Integer, Integer)

-- | make the range be ascending
normalize :: Range -> Range
normalize (x, y) = (min x y, max x y)

-- | whether or an integer is in the range
contains :: Range -> Integer -> Bool
contains r n = n >= a && n <= b where (a, b) = normalize r

-- | wrap around the range (modulus)
(%) :: Integer -> Range -> Integer
n % r = a + mod (n-a) (size r) where a = start r

-- | the number of integers in the range
size :: Range -> Integer
size r = b - a + 1 where (a, b) = normalize r

-- | the ragne as a list
elems :: Range -> [Integer]
elems (x, y) = [x, x + signum (y-x) .. y]

-- | ≤ and ≥
over, under :: Integer -> Range -> Bool
n `over` r = n > b where (_, b) = normalize r
n `under` r = n < a where (a, _) = normalize r

-- | The smaller number and the larger number
start, end :: Range -> Integer
start = fst . normalize
end = snd . normalize

-- | Given a number that should be in the middle range,
-- | either return -1 and the 'underflowed' number
-- | or return +1 and the 'overflowed' number
-- | This is not simple because ranges don't necessarily start at 0.
-- | Note that 'push' does not guaranteen that the resulting number
-- | will be contained by 'lo' or 'hi'. Push should be called
-- | repeatedly (adjusting the three ranges as necessary) until
-- | mid `contains` x.
push :: Integer -> (Range, Range, Range) -> (Integer, Integer)
push x (lo, mid, hi)
--  | push 0 ((1, 30), (1, 31), (1, 30)) ⇒ (-1, 30)
--  | push 0 ((0, 0), (1, 30), (1, 31)) ⇒ (-1, 0)
    | x `under` mid = (-1, x - start mid + end lo + 1)
--  | push 1 ((12, 30), (0, 0), (1, 30)) ⇒ (+1, 1)
--  | push 40 ((1, 31), (1, 30), (0, 30)) ⇒ (+1, 9)
--  | push 40 ((1, 31), (1, 30), (2, 30)) ⇒ (+1, 11)
    | x `over` mid = (1, x - end mid + start hi - 1)
--  | Lookin' good.
    | otherwise = (0, x)
