{-# LANGUAGE QuasiQuotes #-}
module V8Generated.Helpers where
import           Data.List
import           Data.String.Interpolate
import           Data.Bits               (shiftL)
import           DSL.Typed               (Type (..))
import           Generate.Lang
import           Generate.QQ
import           Generate.SMTAST
import           Generate.State

{-|

This file is here because we used to do all DSL-writing as EDSL-writing before
John wrote the parser (!!!). The BrokenIntersect function is still expressed in
the EDSL, though, which is why I haven't eliminated this file.

-}

p :: Program
p = [progFile|src/V8Generated/code.cpp|]

prog_func :: Program -> String -> FunctionDef
prog_func (Program fs _) s = case find (\fd -> fName fd == s) fs of
                                      Just func -> func
                                      Nothing -> error "Couldn't find function"

fn :: String -> FunctionDef
fn = prog_func p

min4 :: FunctionDef
min4 = fn "min4"

max4 :: FunctionDef
max4 = fn "max4"

newRange :: FunctionDef
newRange = fn "newRange"

nanType :: FunctionDef
nanType = fn "nanType"

minusZeroType :: FunctionDef
minusZeroType = fn "minusZeroType"

minusInfinityType :: FunctionDef
minusInfinityType = fn "minusInfinityType"

infinityType :: FunctionDef
infinityType = fn "infinityType"

plainNumberType :: FunctionDef
plainNumberType = fn "plainNumberType"

kIntegerType :: FunctionDef
kIntegerType = fn "kIntegerType"

getBoundary:: FunctionDef
getBoundary = fn "getBoundary"

boundariesSize :: FunctionDef
boundariesSize = fn "BoundariesSize"

anyType :: FunctionDef
anyType = fn "AnyType"

noneType :: FunctionDef
noneType = fn "noneType"

signedAddWouldOverflow :: FunctionDef
signedAddWouldOverflow = fn "SignedAddWouldOverflow32"

-- types helpers

maybeFunc :: FunctionDef
maybeFunc = fn "Maybe"

overlapFunc :: FunctionDef
overlapFunc = fn "Overlap"

-- int min and max 
jsIntMax :: Codegen SExpr
jsIntMax = n Signed (0x7fffffff)

jsIntMaxS :: String
jsIntMaxS = [i| ((int32_t) 0x7fffffff) |]

jsIntMin :: Codegen SExpr
jsIntMin = n Signed (0x80000000)

jsIntMinS :: String
jsIntMinS = [i| ((int32_t) 0x80000000) |]

jsIntMax64 :: Codegen SExpr
jsIntMax64 = n Signed64 2147483647

jsIntMax64S :: String
jsIntMax64S = [i| ((int64_t) 2147483647) |]

jsIntMin64 :: Codegen SExpr
jsIntMin64 = n Signed64 (-2147483648)

jsUIntMax :: Codegen SExpr
jsUIntMax = n Unsigned (0xFFFFFFFF)

jsUIntMin :: Codegen SExpr
jsUIntMin = n Unsigned (0)

-- bitset types

kOtherNumber :: Codegen SExpr
kOtherNumber = n Unsigned (1 `shiftL` 4)

kOtherSigned32 :: Codegen SExpr
kOtherSigned32 = n Unsigned (1 `shiftL` 3)

kNegative31 :: Codegen SExpr
kNegative31 = n Unsigned (1 `shiftL` 6)

kUnsigned30 :: Codegen SExpr
kUnsigned30 = n Unsigned (1 `shiftL` 10)

kOtherUnsigned31 :: Codegen SExpr
kOtherUnsigned31 = n Unsigned (1 `shiftL` 1)

kOtherUnsigned32 :: Codegen SExpr
kOtherUnsigned32 = n Unsigned (1 `shiftL` 2)

kMinusZero :: Codegen SExpr
kMinusZero = n Unsigned (1 `shiftL` 11)

kNaN :: Codegen SExpr
kNaN = n Unsigned (1 `shiftL` 12)
