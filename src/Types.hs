module Types where

import Syntactic

type Name = String

data Type = Placeholder String -- ^ template argument
          | IntType
          | FloatType
          | BoolType
          | StructType Name [(Name, Type)]
          | ProcType [Type]
          | FuncType [Type] [Type]
          | NoneType
          deriving (Show, Eq)


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
