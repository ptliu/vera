module V8Sanity (v8SanityCheck) where
import           ActiveCode.JavaScript
import           BenchUtils
import           Control.Monad.State.Strict    (liftIO)
import           Cpp
import           DSL.DSL                       (SMTResult (..))
import           DSL.Typed                     (Type (..))
import           Generate.Lang
import           Generate.SMTGen
import           Generate.State
import           GenV8
import           V8Generated.Helpers
import           V8Generated.Operations
import           V8Generated.Verify
import           JavaScript
import           Prelude                       hiding (abs, and, floor, max,
                                                min, not, or)
import           Test.Tasty.HUnit
import           Test.Tasty.HUnit
import           Utils
import           Utils

v8SanityCheck = benchTestGroup "V8Sanity" [ 
     --dumbTestTest
         limitUnionTest
                                          , limitIntersectTest
                                          ,typeIntersectTest
                                              , dumbAddTest
                                          , dumbSubtractTest
                                          , dumbMultiplyTest
                                          ]
