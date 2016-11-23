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
          | MatType
          | ProcType ArgTypes
          | FuncType RetTypes ArgTypes
          | NoneType
          deriving (Show, Eq)

type ArgTypes = [Type]
type RetTypes = [Type]


-- | Obtain type from string
strToType :: String -> Type
strToType "int" = IntType
strToType "bool" = BoolType
strToType "float" = FloatType
strToType "mat" = MatType
strToType name = NamedType name


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



-- = Compatibility
-- Two types are compatible if they are equivalent
-- with respect to parameter passing

-- | Tests if two types are compatible
compatible :: Type -> Type -> Bool
compatible (NamedType n1) (StructType n2 _) = n1 == n2
compatible (StructType n1 _) (NamedType n2) = n1 == n2
compatible (ProcType ts1) (ProcType ts2) =
     and $ zipWith compatible ts1 ts2
compatible (FuncType rts1 ats1) (FuncType rts2 ats2) =
    let comp2 = zipWith compatible
     in and (comp2 rts1 rts2) && and (comp2 ats1 ats2)
compatible t1 t2 = t1 == t2


-- = Similarity
-- Two types are similar if it is not possible
-- to exist overloaded objects with both signatures,
-- that is, the interpreter can distinguish them using
-- available contextual information.

-- | Check if types are similar
similar :: Type -> Type -> Bool
similar (ProcType ts1) (ProcType ts2) =
    and $ zipWith compatible ts1 ts2
similar (FuncType _ ats1) (FuncType _ ats2) =
    and $ zipWith compatible ats1 ats2
similar _ _ = False


