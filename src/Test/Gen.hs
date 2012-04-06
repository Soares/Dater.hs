module Properties.Gen
    ( testPrevNext
    , testNextPrev
    ) where
import Gen

testPrevNext :: (Eq a, Gen a) => a -> Bool
testPrevNext a = prev (next a) == a

testNextPrev :: (Eq a, Gen a) => a -> Bool
testNextPrev a = next (prev a) == a
