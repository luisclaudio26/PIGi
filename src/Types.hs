module Types where

type Name = String

class Named a where
    getName :: a -> Name


data Type = Placeholder String -- ^ template argument
          | IntType
          | FloatType
          | BoolType
          | NamedType Name -- struct type, unbinded
          | StructType Name [(Name, Type)]
          | ProcType ArgTypes
          | FuncType RetTypes ArgTypes
          | NoneType
          deriving (Show, Eq)

type ArgTypes = [Type]
type RetTypes = [Type]

-- | List of all template arguments
getPlaceholders :: Type -> [String]
getPlaceholders (Placeholder s) = [s]
getPlaceholders (StructType _ ts) =
    concatMap (getPlaceholders . snd) ts
getPlaceholders (ProcType ts) =
    concatMap getPlaceholders ts
getPlaceholders (FuncType ts ts') =
    concatMap getPlaceholders ts ++ concatMap getPlaceholders ts'
getPlaceholders _ = []


-- | If type is free of template arguments
isConcrete :: Type -> Bool
isConcrete = null . getPlaceholders


-- | Instantiate concrete type with template arguments
-- Nothing if fails
instType :: (String -> Type) -- ^ placeholder to type function
         -> Type -- ^ template type
         -> Type
instType f t =
    let instf = instType f
        instmap = map instf
     in case t of
          (Placeholder s) -> f s
          (ProcType ts) -> ProcType (instmap ts)
          (FuncType ts ts') -> FuncType (instmap ts) (instmap ts')
          (StructType struct ts) ->
              let names = map fst ts
                  types = map (instf . snd) ts
               in StructType struct (zip names types)
          tt -> tt


-- | Things that can be converted to type
class Typed a where
    -- | Convert to type
    toType :: a -> Type


-- | Things that can be converted to a type list
class TypedList a where
    -- | Convert to list of types
    toTypeList :: a -> [Type]


instance (Typed a) => TypedList [a] where
    toTypeList = map toType


-- = Similarity
-- Two types are similar if it is not possible
-- to exist overloaded objects with both signatures,
-- that is, the interpreter can distinguish them using
-- available contextual information.

-- | Check if types are functional similar
-- Two types are functional similar if
--   1. both are function types
--   2. their argument list is of the same type
funcSim :: Type -> Type -> Bool
funcSim (FuncType _ ts1) (FuncType _ ts2) = ts1 == ts2
funcSim _ _ = False



