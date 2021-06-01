{-# LANGUAGE QuasiQuotes #-}
module V8Generated.Operations where
import           Control.Monad
import           Data.List                  (find)
import           DSL.Typed                  (Type (..))
import           Generate.Lang
import           Generate.QQ
import           V8Generated.Helpers
import           Prelude                    hiding (abs, and, div, floor, max,
                                             min, mod, not, or)

{-|

This just hooks up Haskell references to all RA functions with their CPP definitions
in code.cpp

-}

test :: FunctionDef
test = fn "test"

test2 :: FunctionDef
test2 = fn "test2"


-- TODO -- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
addRanger :: FunctionDef
addRanger = fn "AddRanger"

subtractRanger :: FunctionDef
subtractRanger = fn "SubtractRanger"

multiplyRanger :: FunctionDef
multiplyRanger = fn "MultiplyRanger"

-- Limits functions

limitsUnion :: FunctionDef
limitsUnion = fn "Union"

limitsIntersect :: FunctionDef
limitsIntersect = fn "LimitIntersect"

isEmpty :: FunctionDef
isEmpty = fn "IsEmpty"

copy :: FunctionDef
copy = fn "copy"

-- Bitset Functions

bitsetMin :: FunctionDef
bitsetMin = fn "BitsetMin"

bitsetIs :: FunctionDef
bitsetIs = fn "BitsetIs"

bitsetIsNone :: FunctionDef
bitsetIsNone = fn "BitsetIsNone"

bitsetLub :: FunctionDef
bitsetLub = fn "BitsetLub"

bitsetGlb :: FunctionDef
bitsetGlb = fn "BitsetGlb"

bitsetTypeLub :: FunctionDef
bitsetTypeLub = fn "BitsetTypeLub"

bitsetTypeGlb :: FunctionDef
bitsetTypeGlb = fn "BitsetTypeGlb"

numberBits :: FunctionDef
numberBits = fn "NumberBits"

-- Type Functions

typeIntersect :: FunctionDef
typeIntersect = fn "Intersect"

typeIntersectAux :: FunctionDef
typeIntersectAux = fn "IntersectAux"

isBitset :: FunctionDef
isBitset = fn "IsBitset"

asBitset :: FunctionDef
asBitset = fn "AsBitset"

newBitset :: FunctionDef
newBitset = fn "NewBitset"

typeIs :: FunctionDef
typeIs = fn "Is"

typeSlowIs :: FunctionDef
typeSlowIs = fn "SlowIs"

typeIsNone :: FunctionDef
typeIsNone = fn "TypeIsNone"

typeIsAny :: FunctionDef
typeIsAny = fn "TypeIsAny"

typeIsUnion :: FunctionDef
typeIsUnion = fn "IsUnion"

typeIsRange :: FunctionDef
typeIsRange = fn "IsRange"

typeIsTuple :: FunctionDef
typeIsTuple = fn "IsTuple"

rangeContains :: FunctionDef
rangeContains = fn "RangeContains"

simplyEquals :: FunctionDef
simplyEquals = fn "SimplyEquals"

getLimits :: FunctionDef
getLimits = fn "getLimits"
