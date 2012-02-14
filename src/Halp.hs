{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Halp where
import Calendar
import Control.Applicative
import Data.Ratio hiding ((%))
import Data.Vec ( Vec, VecList, Fold, Head, Tail, Map, ZipWith, (:.)(..), toList, fromList )
import Data.Vec.Nat
import qualified Data.Vec as Vec
import Prelude hiding ((!!))
import Range hiding (mod)
import Utils

-- | We have a datatype that Foo uses to generate data
-- | In actuallity, there are a few of these, and they are oft complex
type Something = Int

-- | Foos generate data. For now, we'll pretend that the data is homogeneous.
type Data = Int

-- | Foo is used for generating both static data and vectors of data.
-- | The size of the vector needs to be parametized.
data Foo n = Foo
    { static  :: Something -> Data
    , dynamic :: (NList n Data v) => Something -> v
    }

-- | Sometimes we want to directly manipulate the unwrapped data.
data (NList n Data v) => Unwrapped n v = Unwrapped
    { uStatic  :: Data
    , uDynamic :: v
    }

-- | Would love to be able to avoid this.
class (Vec n a v, Fold v a, VecList a v, ZipWith a a a v v v) => NList n a v
instance NList N1 a (a :. ())
instance NList N2 a (Vec.Vec2 a)
instance NList N3 a (Vec.Vec3 a)
-- (and so on)

-- | These are some functions that operate on foo data.
-- | Why do I need the list requirement here?
fooProd :: NList n Data v => Foo n -> Int -> Int
fooProd f = Vec.product . dynamic f

-- | This is a function that operates on the parts.
-- | Ideally, we shouldn't need to use toList and fromList here,
-- | but I haven't figured out how to cleanly parameterize
-- | the requirements.
integratedParts :: NList n Data v => Foo n -> Something -> v
integratedParts f s = fromList . modify . reverse . toList $ dynamic f s where
    modify = foldl (+) s

-- | I don't need NList here.
staticPart :: Foo n -> Something -> Data
staticPart f s = s + static f s

-- | Get the foo into an operational state
unwrap :: NList n Data v => Foo n -> 

-- | Split a rational into its component parts
breakDown :: NList n Integer v => Common n -> Rational -> DateTime n v
breakDown f r = DateTime y m d (smallPart f r) (detailPart f r)
    where (y, m, d) = largePart f r

-- | Rebuild a rational from its component parts
rebuild :: NList n Integer v => Common n -> DateTime n v -> Rational
rebuild f (DateTime y m d t x) = a + b + c where
    a = toRational $ numDays f (y, m, d)
    b = dayFraction f (y, m, d) t
    c = x % timeUnitsPerDay f (y, m, d)

-- | Adjust the year and month until they are sane, i.e.
-- | 12/-1 becomes 11/12
normalizeYM :: Common n -> Year -> Month -> (Year, Month)
normalizeYM f y m
    | months f y `contains` m = (y, m)
    | otherwise = normalizeYM f (y + delta) m'
    where (delta, m') = push m (around (months f) y)

-- | Adjust the year, month, and day until it is sane, i.e.
-- | 12/0/-1 becomes 11/12/30
normalizeYMD :: Common n -> YMD -> YMD
normalizeYMD f (y, m, d)
    | days f ny nm `contains` d = (ny, nm, d)
    | otherwise = normalizeYMD f (ny, nm + delta, d')
    where (delta, d') = push d (around (days f ny) nm)
          (ny, nm) = normalizeYM f y m

-- | Turn a YMD into the number of days since 'the beginning'
numDays :: Common n -> YMD -> Integer
numDays f ymd = ydays + mdays + ddays where
    ydays = sum $ map (daysInYear f) ys
    ys = if y >= 0 then [0..y-1] else [-1,-2..y]
    mdays = sum $ map (size . days f y) ms
    ms = filter (m >) (elems $ months f y)
    ddays = toInteger $ length $ filter (d >) (elems $ days f y m)
    (y, m, d) = normalizeYMD f ymd

-- | Turn a Time into a fraction of a day
dayFraction :: NList n Integer v => Common n -> YMD -> v -> Rational
dayFraction f ymd ts = timeInSeconds % timeUnitsPerDay f ymd where
    timeInSeconds = Vec.sum $ Vec.zipWith (*) ts mods
    -- TODO: mods = drop 1 $ prods $ timeSplits f ymd
    mods = timeSplits f ymd

instance (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) => Calendar (Common n) where
    data Delta (Common n) = Delta
        { dYear   :: Maybe Year
        , dMonth  :: Maybe Month
        , dDay    :: Maybe Day
        , dTime   :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) => v
        , dDetail :: Maybe Detail
        }

    display _ _ = [] -- TODO
    parse _ _ = [] -- TODO

    plus = change madd
    minus = change msub
    clobber = change mright

    normalize c r = r + beginning c
    denormalize c r = r - beginning c

-- TODO
instance Read (Common n) where
    readsPrec _ _ = []
instance Show (Common n) where
    show _ = []
instance Read (Delta (Common n)) where
    readsPrec _ _ = []
instance Show (Delta (Common n)) where
    show _ = []

type Operation = Integer -> Maybe Integer -> Integer

madd, msub, mright :: Operation
madd i = maybe i (i +)
msub i = maybe i (i -)
mright i = maybe i id

operation :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) =>
             (Integer -> Maybe Integer -> Integer) ->
             DateTime n u -> Delta (Common n) -> DateTime n u
operation op (DateTime y m d (ts :: u) x) (Delta my mm md (mt :: v) mx) = DateTime
    (op y my) (op m mm) (op d md)
    (Vec.zipWith op ts mt)
    (op (numerator x) (numerator <$> mx) % op (denominator x) (denominator <$> mx))

change :: (NList n (Maybe Integer) v, NList n Integer u, ZipWith Integer (Maybe Integer) Integer u v u) =>
          (Integer -> Maybe Integer -> Integer) ->
          Common n -> Rational -> Delta (Common n) -> Rational
change op c r = rebuild c . operation op (breakDown c r)

{-
fake :: EarthlikeFormat -> Relative -> Rational
fake f rel = rebuild f (y, m, d, ts, top % bot) where
    y:m:d:xs = pad 0 8 $ map (fromMaybe 0) rel
    ts = init $ init xs
    top = last $ init xs
    bot = last xs
-}

