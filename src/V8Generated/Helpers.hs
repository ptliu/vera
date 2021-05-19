{-# LANGUAGE QuasiQuotes #-}
module V8Generated.Helpers where
import           Data.List
import           Data.String.Interpolate
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
