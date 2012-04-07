{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Test.Data.DateTime.ConstPart where
import Data.DateTime.ConstPart
import Data.Modable
import Data.Normalize
import Test.Data.Enum
import Test.Data.Coded
import Test.Data.Modable
import Test.Data.Normalize
import Test.Data.Pair
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testConstPart :: forall a b.
    ( Show a, Show b
    , Arbitrary a, Arbitrary b
    , Modable a, Modable b
    , Eq (Relative a), Eq (Relative b)
    , Show (Relative a), Show (Relative b)
    , Arbitrary (Relative a), Arbitrary (Relative b)
    , Composable a b
    ) => (a:::b) -> Test
testConstPart t = testGroup "time composition"
    [ testEnum t
    , testCoded t
    , testModable t
    , testNormal t
    , testPair t
    , testProperty "reconstructible" (\(a::(a:::b)) ->
        let (o, b) = normalize a
            size = succ (maxBound - minBound :: (a:::b))
            x = o * fromIntegral size
        in (x + fromIntegral b) == fromIntegral a)
    ]
