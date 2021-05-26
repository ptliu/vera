module V8Generated.Verify where
import           Control.Monad                 (forM_)
import           Control.Monad.State.Strict    (liftIO)
import           Data.List                     (isInfixOf)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import           DSL.DSL                       (isSat, isUnsat)
import           DSL.Typed                     (SMTResult (..), Type (..))
import           Generate.Lang
import           Generate.SMTAST
import           Generate.SMTGen
import           Generate.State
import           GHC.Float
import           V8Generated.Helpers
import           V8Generated.Objects
import           V8Generated.Operations
import           Prelude

-- This is just to test the Limits helper functions, just for sanity
fInLimits :: FunctionDef
fInLimits =
  let args = [ ("fval", t Double)
             , ("frange", c "limits")
             ]
      body = [ return_ $ ((v "fval" .=>. (v "frange" .->. "min")) .&&. (v "fval" .<=. (v "frange" .->. "max")))
             ]
  in Function "fInLimits" (t Bool) args body

--
-- Automatic testing infrastructure
--

data TestFunction = Binary { testName :: String
                           , binaryCppOp :: FunctionDef
                           , binaryJSOp :: (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                           }
                  | Constant { testName :: String
                             , constCppOp :: FunctionDef
                             , constJSOp :: (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                             }
                  | Unary { testName   :: String
                          , unaryCppOp :: FunctionDef
                          , unaryJSOp  :: (Codegen SExpr -> Codegen SExpr)
                          }
                  | Set { testName :: String
                        , setOp    :: FunctionDef
                        }

isBinary :: TestFunction -> Bool
isBinary Binary{} = True
isBinary _        = False

isConstant :: TestFunction -> Bool
isConstant Constant{} = True
isConstant _          = False

isSet :: TestFunction -> Bool
isSet Set{} = True
isSet _     = False

-- simple limits sanity tests

testLimitUnion :: TestFunction -> Codegen ()
testLimitUnion fn = do
  setupLimitsSet (setOp fn) (testName fn)
  genBodySMT [vcall "verifyLimitUnion" [v "isInLeft", v "isInRight", v "isInResult"]]

verifyLimitUnion :: FunctionDef
verifyLimitUnion =
  let args = [ ("isInLeft", t Bool)
             , ("isInRight", t Bool)
             , ("isInResult", t Bool)
             ]
      body = [ push_
             -- assert precond
             -- assert not postcond
             -- yay :D
             , assert_ $ (v "isInLeft") .||. (v "isInRight")
             , assert_ $ not_ $ v "isInResult"
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify limits Union" r
             , pop_
             ]
  in Function "verifyLimitUnion" Void args body

-- isNan test

testAddRanger :: TestFunction -> Codegen ()
testAddRanger fn = do
    {-setupAllFloat fn-}
  setupAllTest fn
  genBodySMT [vcall "verifyAddRanger" [v "lhs_min", v "lhs_max", v "rhs_min", v "rhs_max", v "result_type"]]

verifyAddRanger :: FunctionDef
verifyAddRanger =
  let args = [ ("lhs_min", t Double)
             , ("lhs_max", t Double)
             , ("rhs_min", t Double)
             , ("rhs_max", t Double)
             , ("result_type", c "v8type")
             ]
      body = [ push_
             -- assert precond
             -- assert not postcond
             -- yay :D
             , assert_ $ not_ $ ((isInf $ v "lhs_min") .&&. (isInf $ v "rhs_min") .&&. (isInf $ v "lhs_max") .&&. (isInf $ v "rhs_max") .&&. (isNeg $ v "lhs_min") .&&. (isNeg $ v "lhs_max") .&&. (not_ $ isNeg $ v "rhs_min") .&&. (not_ $ isNeg $ v "rhs_max")) `testImplies` (v "result_type" .->. "maybeNaN")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify AddRanger" r
             , pop_
             ]
  in Function "verifyAddRanger" Void args body

testSubtractRanger :: TestFunction -> Codegen ()
testSubtractRanger fn = do
    {-setupAllFloat fn-}
  setupAllTest fn
  genBodySMT [vcall "verifySubtractRanger" [v "lhs_min", v "lhs_max", v "rhs_min", v "rhs_max", v "result_type"]]

verifySubtractRanger :: FunctionDef
verifySubtractRanger =
  let args = [ ("lhs_min", t Double)
             , ("lhs_max", t Double)
             , ("rhs_min", t Double)
             , ("rhs_max", t Double)
             , ("result_type", c "v8type")
             ]
      body = [ push_
             -- assert precond
             -- assert not postcond
             -- yay :D
             , assert_ $ not_ $ ((isInf $ v "lhs_min") .&&. (isInf $ v "rhs_min") .&&. (isInf $ v "lhs_max") .&&. (isInf $ v "rhs_max") .&&. (isNeg $ v "lhs_min") .&&. (isNeg $ v "lhs_max") .&&. (isNeg $ v "rhs_min") .&&. (isNeg $ v "rhs_max")) `testImplies` (v "result_type" .->. "maybeNaN")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify SubtractRanger" r
             , pop_
             ]
  in Function "verifySubtractRanger" Void args body

testMultiplyRanger :: TestFunction -> Codegen ()
testMultiplyRanger fn = do
    {-setupAllFloat fn-}
  setupAllTest fn
  genBodySMT [vcall "verifyMultiplyRanger" [v "lhs_min", v "lhs_max", v "rhs_min", v "rhs_max", v "result_type"]]

verifyMultiplyRanger :: FunctionDef
verifyMultiplyRanger =
  let args = [ ("lhs_min", t Double)
             , ("lhs_max", t Double)
             , ("rhs_min", t Double)
             , ("rhs_max", t Double)
             , ("result_type", c "v8type")
             ]
      body = [ push_
             -- assert precond
             -- assert not postcond
             -- yay :D
             , assert_ $ not_ $ ((isInf $ v "lhs_min") .&&. (isInf $ v "rhs_min") .&&. (isInf $ v "lhs_max") .&&. (isInf $ v "rhs_max") .&&. (isZero $ v "rhs_max")) `testImplies` (v "result_type" .->. "maybeNaN")
             , expect_ isUnsat $ \r -> showInt32Result "Failed to verify MultiplyRanger" r
             , pop_
             ]
  in Function "verifyMultiplyRanger" Void args body

-- Int32 verification conditions

-- Float verification conditions






-- General setup functions

setupAlli32 :: TestFunction -> Codegen ()
setupAlli32 fn = do
  if isBinary fn
  then setupi32 (binaryCppOp fn) (testName fn) (binaryJSOp fn)
  else if isConstant fn
       then setupConstanti32 (constCppOp fn) (testName fn) (constJSOp fn)
       else setupUnaryi32 (unaryCppOp fn) (testName fn) (unaryJSOp fn)

setupAllFloat :: TestFunction -> Codegen ()
setupAllFloat fn = do
  if isBinary fn
  then setupFloat (binaryCppOp fn) (testName fn) (binaryJSOp fn)
  else if isSet fn
       then setupSetOp (setOp fn) (testName fn)
       else setupUnaryFloat (unaryCppOp fn) (testName fn) (unaryJSOp fn)

setupAllTest :: TestFunction -> Codegen ()
setupAllTest fn = do
  setupRanger (setOp fn) (testName fn)

-- Individual setup functions

setupUnaryi32 :: FunctionDef
              -> String
              -> (Codegen SExpr -> Codegen SExpr)
              -> Codegen ()
setupUnaryi32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "start_range"
              , declare (t Signed) "start"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "start_range")   `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "start", v "start_range"]
              , (v "result") `assign` (fn $ v "start")
              , expect_ isSat (error "Has to start out SAT")
              ]
  -- Once we generate the SMT, we can verify each condition
  genBodySMT verif

setupUnaryFloat :: FunctionDef
                -> String
                -> (Codegen SExpr -> Codegen SExpr)
                -> Codegen ()
setupUnaryFloat op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "start_range"
              , declare (t Double) "start"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , (v "start_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range"]
              , assert_ $ call "fInRange" [v "start", v "start_range"]
              , v "start" `assign` v "start"
              , (v "result") `assign` (fn $ v "start")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupConstanti32 :: FunctionDef
                 -> String
                 -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
                 -> Codegen ()
setupConstanti32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "start_range"
              , declare (t Signed) "start"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "start_range")   `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "start_range", v "right"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "start", v "start_range"]
              , (v "result") `assign` (v "start" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupi32 :: FunctionDef
         -> String
         -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
         -> Codegen ()
setupi32 op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (t Signed) "left"
              , declare (c "range") "right_range"
              , declare (t Signed) "right"
              , declare (c "range") "result_range"
              , declare (t Signed) "result"
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , assert_ $ call "vInRange" [v "left", v "left_range"]
              , assert_ $ call "vInRange" [v "right", v "right_range"]
              , (v "result") `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupFloat :: FunctionDef
           -> String
           -> (Codegen SExpr -> Codegen SExpr -> Codegen SExpr)
           -> Codegen ()
setupFloat op fnName fn = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (t Double) "left"
              , declare (c "range") "right_range"
              , declare (t Double) "right"
              , declare (c "range") "result_range"
              , declare (t Double) "result"
              , declare (t Double) "lefty"
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]
                -- Actually perform the JS operation
              , assert_ $ call "fInRange" [v "left", v "left_range"]
              , assert_ $ call "fInRange" [v "right", v "right_range"]
              , v "left" `assign` v "left"
              , v "lefty" `assign` v "left"
              , v "right" `assign` v "right"
              , v "result" `assign` (v "left" `fn` v "right")
              , expect_ isSat (error "Has to start out SAT")
              ]
  genBodySMT verif

setupSetOp :: FunctionDef
           -> String
           -> Codegen ()
setupSetOp op fnName = do
  defineAll op
  let verif = [ declare (c "range") "left_range"
              , declare (c "range") "right_range"
              , (v "left_range")   `assign` (call "wellFormedRange" [])
              , (v "right_range")  `assign` (call "wellFormedRange" [])

              , declare (c "range") "result_range"
              , (v "result_range") `assign` call fnName [v "left_range", v "right_range"]

              , declare (t Double) "elem"
              , declare (t Bool) "isInRight"
              , declare (t Bool) "isInLeft"
              , declare (t Bool) "isInResult"

              , v "isInRight" `assign` (call "fInRange" [v "elem", v "right_range"])
              , v "isInLeft" `assign` (call "fInRange" [v "elem", v "left_range"])
              , v "isInResult" `assign` (call "fInRange" [v "elem", v "result_range"])
              , expect_ isSat (error "Should be sat")
              ]
  genBodySMT verif

setupLimits :: FunctionDef
          -> String
          -> Codegen ()
setupLimits op fnName = do
  defineAll op
  let verif = [ declare (c "limits") "lhs"
              , declare (c "limits") "rhs"
              , declare (c "limits") "result_limit"

              , (v "result_limit") `assign` call fnName [v "lhs", v "rhs"]
              , expect_ isSat (error "Should be sat")
              ]
  genBodySMT verif

setupLimitsSet :: FunctionDef
          -> String
          -> Codegen ()
setupLimitsSet op fnName = do
  defineAll op
  let verif = [ declare (c "limits") "lhs"
              , declare (c "limits") "rhs"
              , declare (c "limits") "result_limit"

              , (v "result_limit") `assign` call fnName [v "lhs", v "rhs"]

              , declare (t Double) "elem"
              , declare (t Bool) "isInRight"
              , declare (t Bool) "isInLeft"
              , declare (t Bool) "isInResult"

              , v "isInRight" `assign` (call "fInLimits" [v "elem", v "rhs"])
              , v "isInLeft" `assign` (call "fInLimits" [v "elem", v "lhs"])
              , v "isInResult" `assign` (call "fInLimits" [v "elem", v "result_limit"])
              , expect_ isSat (error "Should be sat")
              ]
  genBodySMT verif


        

setupRanger :: FunctionDef
          -> String
          -> Codegen ()
setupRanger op fnName = do
  defineAll op
  let verif = [ declare (t Double) "lhs_min"
              , declare (t Double) "lhs_max"
              , declare (t Double) "rhs_min"
              , declare (t Double) "rhs_max"
              
              , declare (c "v8type") "result_type"
              , (v "result_type") `assign` call fnName [v "lhs_min", v "lhs_max", v "rhs_min", v "rhs_max"]
              , expect_ isSat (error "Should be sat")
              ]
  genBodySMT verif

defineAll op = do
  class_ v8type
  class_ limits
  define op
  define verifyAddRanger
  define verifySubtractRanger
  define verifyMultiplyRanger
  define min4
  define max4
  define newRange
  define nanType
  -- limits
  define verifyLimitUnion
  define isEmpty
  define copy
  -- checkers
  define fInLimits

---
--- Printing
---

data VerifResult = Verified
                 | UnsatImpl
                 | OverlappingRange { counterexample :: M.Map String Double }
                 | BadLowerBound { counterexample :: M.Map String Double }
                 | BadUpperBound { counterexample :: M.Map String Double }
                 | UndefRange { counterexample :: M.Map String Double }
                 | NoNanFlag { counterexample :: M.Map String Double }
                 | NoInfFlag { counterexample :: M.Map String Double }
                 | NoNegzFlag { counterexample :: M.Map String Double }
                 deriving (Eq, Ord)

instance Show VerifResult where
    show (OverlappingRange ce) = "Upper and lower of result range may overlap\n:" ++
                                 prettyCounterexampleInts ce
    show (BadLowerBound ce)    = "Example operation can be outside of lower boud\n:" ++
                                 prettyCounterexampleInts ce
    show (BadUpperBound ce)    = "Example operation can be outside of upper bound\n" ++
                                 prettyCounterexampleInts ce
    show (UndefRange ce)       = "Example operation may introduce undefined behavior:\n" ++
                                 prettyCounterexampleInts ce
    show (NoNanFlag ce)        = "Example operation returns NAN without flag set:\n" ++
                                 (unlines $ getNanList ce)
    show (NoInfFlag ce)        = "Example operation returns INF without flag set:\n" ++
                                 (unlines $ getNanList ce)
    show (NoNegzFlag ce)       = "Example operation returns -0 without flag set:\n" ++
                                 (unlines $ getNegzList ce)
    show Verified              = "Verified!"
    show UnsatImpl             = "Verification failed (e.g., due to a timeout)"

showExpResult :: String -> SMTResult -> IO ()
showExpResult str result = error $ str ++ "\n" ++ (unlines $ getExpList $ example result)

getExpList :: M.Map String Double -> [String]
getExpList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "testy" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show fl --(round fl :: Integer)
                                 ]

showNanResult :: String -> SMTResult -> IO ()
showNanResult str result = error $ str ++ "\n" ++ (unlines $ getNanList $ example result)

getNanList :: M.Map String Double -> [String]
getNanList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "copy_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "isNan" `isInfixOf` str -> sstr str fl
                         _ | "jsSign" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "result_" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if fl /= fl
                                            then "NaN"
                                            else show fl --(round fl :: Integer)
                                 ]

showInfResult :: String -> SMTResult -> IO ()
showInfResult str result = error $ str ++ "\n" ++ (unlines $ getInfList $ example result)

getInfList :: M.Map String Double -> [String]
getInfList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "abs_res" `isInfixOf` str -> sstr str fl
                         _ | "abs_after" `isInfixOf` str -> sstr str fl
                         _ | "left_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "start_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "result_range_maxExponent" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                       ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isInfinite fl
                                            then "Inf"
                                            else show (round fl :: Integer)
                                 ]

showNegzResult :: String -> SMTResult -> IO ()
showNegzResult str result = error $ str ++ "\n" ++ (unlines $ getNegzList $ example result)

getNegzList :: M.Map String Double -> [String]
getNegzList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "sbs" `isInfixOf` str -> sstr str fl
                         _ | "left_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "left_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "right_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "start_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32UpperBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_hasInt32LowerBound" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canBeNegativeZero" `isInfixOf` str -> sstr str fl
                         _ | "start_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "start_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "result_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "result_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "left_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "left_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_range_lower" `isInfixOf` str -> sstr str fl
                         _ | "right_range_upper" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "jsCeilStart" `isInfixOf` str -> sstr str fl
                         _ | "jsCeilResult" `isInfixOf` str -> sstr str fl
                         _ | "jsSign" `isInfixOf` str -> sstr str fl
                         _ | "jsSignStart" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isNegativeZero fl
                                            then "negz"
                                            else show fl
                                 ]

showFractResult :: String -> SMTResult -> IO ()
showFractResult str result = error $ str ++ "\n" ++ (unlines $ getFractList $ example result)

getFractList :: M.Map String Double -> [String]
getFractList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "result_fract" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ | "left_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "right_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "start_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "result_range_canHaveFractionalPart" `isInfixOf` str -> sstr str fl
                         _ | "right_1" `isInfixOf` str -> sstr str fl
                         _ | "left_1" `isInfixOf` str -> sstr str fl
                         _ | "start_1" `isInfixOf` str -> sstr str fl
                         _ | "result_1" `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", if isNegativeZero fl
                                            then "negz"
                                            else show fl
                                 ]

showInt32Result :: String -> SMTResult -> IO ()
showInt32Result str result =
  case result of
    SolverSat e ->
      let list = unlines $ getIntList $ e
      in error $ str ++ "\n" ++ list
    SolverUnsat -> error "Unsat result"
    SolverFailed -> error "Solver failed (e.g., due to timeout)"

getIntList :: M.Map String Double -> [String]
getIntList fls = catMaybes $ map (\(str, fl) ->
                       case str of
                         _ | "undef" `isInfixOf` str -> Nothing
                         _ | "value"  `isInfixOf` str -> sstr str fl
                         _ | "result"  `isInfixOf` str -> sstr str fl
                         _ -> Nothing
                     ) $ M.toList fls
  where
    sstr str fl = Just $ unwords [str, ":", show fl] --show (round fl :: Integer)]



prettyCounterexampleInts :: M.Map String Double
                         -> String
prettyCounterexampleInts ce = unlines $ getIntList ce
