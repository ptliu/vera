{-# LANGUAGE QuasiQuotes #-}
module V8Generated.Operations ( addRanger
                              , subtractRanger
                              , multiplyRanger
                                     ) where
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

-- TODO -- | https://searchfox.org/mozilla-central/source/js/src/jit/RangeAnalysis.cpp#744
addRanger :: FunctionDef
addRanger = fn "AddRanger"

subtractRanger :: FunctionDef
subtractRanger = fn "SubtractRanger"

multiplyRanger :: FunctionDef
multiplyRanger = fn "MultiplyRanger"

isEmpty :: FunctionDef
isEmpty = fn "isEmpty"

