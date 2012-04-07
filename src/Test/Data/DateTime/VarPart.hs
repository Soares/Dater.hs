{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Test.Data.DateTime.VarPart where
import Data.DateTime.VarPart
import Data.Modable
import Data.Normalize
import Data.Zeroed
import Test.Data.Enum
import Test.Data.Coded
import Test.Data.Modable
import Test.Data.Normalize
import Test.Data.Pair
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testVarPart :: forall a b.
    ( Show a, Show b
    , Arbitrary a, Arbitrary b
    , Modable a, Modable b
    , Eq (Relative a), Eq (Relative b)
    , Show (Relative a), Show (Relative b)
    , Arbitrary (Relative a), Arbitrary (Relative b)
    , Zeroed a
    , Composable a b
    ) => (a:/:b) -> Test
testVarPart d = testGroup "date composition"
    [ testEnum d
    , testCoded d
    , testModable d
    , testNormal d
    , testPair d
    , testProperty "no overflow" (\(a::(a:/:b)) -> fst (normalize a) == 0)
    ]
