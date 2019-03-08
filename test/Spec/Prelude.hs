module Spec.Prelude (
    TestTree, testGroup, testCase, (===)
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)


(===) :: (Eq a, Show a) => a -> a -> Assertion
a === b = a @?= b

infix 1 ===
