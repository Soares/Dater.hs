{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Pair
    ( Pair(..)
    , fst
    , snd
    , curry
    , uncurry
    , convert
    , merge
    , first
    , second
    , (***)
    , (&&&)
    ) where
import Prelude hiding ((.), fst, snd, curry, uncurry, id)
import qualified Prelude
import Control.Applicative
import Control.Arrow (Arrow, arr)
import Control.Monad (join)
import Control.Category ((.), (>>>), id)
import Data.Calendar.Utils
import qualified Control.Arrow as Arr

-- | Things that can act like a two-tuple
class Pair (-&) where
    toTuple :: (a -& b) -> (a, b)
    fromTuple :: (a, b) -> (a -& b)
    fromTuple = uncurry build
    build :: a -> b -> (a -& b)
    build = fromTuple .: (,)

-- | Pair-compatable prelude functions

fst :: Pair (-&) => (a -& b) -> a
fst = Prelude.fst . toTuple

snd :: Pair (-&) => (a -& b) -> b
snd = Prelude.snd . toTuple

dup :: Pair (-&) => a -> (a -& a)
dup = join build

curry :: Pair (-&) => ((a -& b) -> c) -> a -> b -> c
curry = flip (.:) build

uncurry :: Pair (-&) => (a -> b -> c) -> (a -& b) -> c
uncurry f = f <$> fst <*> snd

-- | Convert one pair type into another
convert :: (Pair (-&), Pair (~&)) => (a -& b) -> (a ~& b)
convert = fromTuple . toTuple

-- | Merge a pair (of functions) with a pair (of parameters)
-- | generating a new pair
merge :: (Arrow (~>), Pair (-&)) => (a ~> x) -& (b ~> y) -> (a -& b) ~> (x -& y)
merge f = (fst f) *** (snd f)

-- | Control.Arrow's tuple-dependant functions, defined in terms
-- | of pairs.
first :: (Arrow (~>), Pair (-&)) => (a ~> b) -> (a -& x) ~> (b -& x)
first f = f *** id

second :: (Arrow (~>), Pair (-&)) => (a ~> b) -> (x -& a) ~> (x -& b)
second f = id *** f

(&&&) :: forall a x y (~>) (-&).  (Arrow (~>), Pair (-&))
    => (a ~> x) -> (a ~> y) -> a ~> (x -& y)
f &&& g = arr (dup :: a -> a -& a) >>> first f >>> second g

(***) :: forall a b x y (~>) (-&). (Arrow (~>), Pair (-&))
    => (a ~> x) -> (b ~> y) -> (a -& b) ~> (x -& y)
f *** g = arr toTuple >>> f Arr.*** g >>> arr fromTuple

-- | The simple tuple instance
instance Pair (,) where
    toTuple = id
    fromTuple = id
