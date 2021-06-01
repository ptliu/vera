module GenV8 where

import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           V8Generated.Helpers
import           V8Generated.Operations
import           V8Generated.Verify
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Utils

genV8Tests :: BenchTest
genV8Tests = benchTestGroup "V8Verification"
                    [ dumbAddTest
                    , dumbSubtractTest
                    , dumbMultiplyTest
                    , numberAddTest
                    ]

-- limits tests
limitUnionTest :: BenchTest
limitUnionTest = benchTestCase "LimitsUnion" $ evalCodegen Nothing $ testLimitUnion $ Set "Union" limitsUnion 

limitIntersectTest :: BenchTest
limitIntersectTest = benchTestCase "LimitsIntersect" $ evalCodegen Nothing $ testLimitIntersect $ Set "LimitIntersect" limitsIntersect

-- bitset tests
--bitsetMinTest :: BenchTest
--bitsetMinTest = benchTestCase "BitsetMin" $ evalCodegen Nothing $ testBitsetMin $ Set "BitsetMin" bitsetMin

-- ranger tests

dumbAddTest :: BenchTest
dumbAddTest = benchTestCase "AddRanger" $ evalCodegen Nothing $ testAddRanger $ Set "AddRanger" addRanger

dumbSubtractTest :: BenchTest
dumbSubtractTest = benchTestCase "SubtractRanger" $ evalCodegen Nothing $ testSubtractRanger $ Set "SubtractRanger" subtractRanger

dumbMultiplyTest :: BenchTest
dumbMultiplyTest = benchTestCase "MultiplyRanger" $ evalCodegen Nothing $ testMultiplyRanger $ Set "MultiplyRanger" multiplyRanger

-- type tests
typeIntersectTest :: BenchTest
typeIntersectTest = benchTestCase "TypeIntersect" $ evalCodegen Nothing $ testTypeIntersect $ Set "Intersect" typeIntersect

dumbTestTest :: BenchTest
dumbTestTest = benchTestCase "DumbTestTest" $ evalCodegen Nothing $ testTest $ Set "test2" test2

-- operation tests
numberAddTest :: BenchTest
numberAddTest = benchTestCase "NumberAdd" $ evalCodegen Nothing $ testNumberAdd $ Binary "NumberAdd" numberAdd jsAdd

    {-mkFloatTests :: String -> String -> TestFunction -> BenchTest
mkFloatTests outerName testGroupName testFn =
  benchTestGroup outerName
    [ makeTest (testGroupName ++ " inf") $ testInf testFn
    , makeTest (testGroupName ++ " nan") $ testNan testFn
    , makeTest (testGroupName ++ " negative zero") $ testNegZ testFn
    , makeTest (testGroupName ++ " fract") $ testFract testFn
    , makeTest (testGroupName ++ " lower i32") $ testLower testFn
    , makeTest (testGroupName ++ " float lower i32") $ testFlLower testFn
    , makeTest (testGroupName ++ " upper i32") $ testUpper testFn
    , makeTest (testGroupName ++ " float upper i32") $ testFlUpper testFn
    , makeTest (testGroupName ++ " exp") $ testExp testFn
    , makeTest (testGroupName ++ " WF hasBound invariant") $ testBoundInvariants testFn
    , makeTest (testGroupName ++ " WF bounds between min max") $ testBoundForm testFn
    , makeTest (testGroupName ++ " WF valid exp") $ testExpForm testFn
    , makeTest (testGroupName ++ " WF valid exp/bound pair") $ testExpBounds testFn
    , makeTest (testGroupName ++ " UB") $ testUB testFn
    ]
  where makeTest str act = benchTestCase str $ evalCodegen Nothing act-}
