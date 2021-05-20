module V8Generated.Objects where
import           DSL.Typed     (Type (..))
import           Generate.Lang

-- | Set up invariants for our input range. This is not an IonMonkey function
-- TODO

-- | The limits object
limits :: ClassDef
limits = let fields = [ ("min", Double)
                      , ("max", Double)
                      ]
        in ClassDef "limits" fields []

-- | The V8 Type object
v8type :: ClassDef
v8type = let fields = [ ("bitset", Unsigned)
                      , ("hasRange", Bool)
                      , ("min", Double)
                      , ("max", Double)
                      , ("maybeNaN", Bool)
                      , ("maybeMinusZero", Bool)
                      , ("isUnion", Bool)
                      ]
        in ClassDef "v8type" fields []
