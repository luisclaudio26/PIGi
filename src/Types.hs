module Types where

type Name = String

class Named a where
    getName :: a -> Name

-- = Types

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

type ArgTypes = [AnnotatedType]
type RetTypes = [Type]


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


-- | Obtain type from string
strToType :: String -> Type
strToType "int" = IntType
strToType "bool" = BoolType
strToType "float" = FloatType
strToType "mat" = MatType
strToType name = NamedType name


-- = Type annotations

data Annotation = Ref | Const | Mut
    deriving (Show, Eq)


data AnnotatedType = AnnotatedType Type [Annotation]
    deriving (Show, Eq)


-- | Things that can be converted to an annotated type
class AnnotatedTyped a where
    -- | Convert to annotated type
    toAnnType :: a -> AnnotatedType


instance Typed AnnotatedType where
    toType (AnnotatedType t _) = t


instance AnnotatedTyped Type where
    toAnnType t = AnnotatedType t [] 


-- | Apply type transformation to annotated type
mapAnn :: (Type -> Type) -> AnnotatedType -> AnnotatedType
mapAnn f (AnnotatedType t as) = AnnotatedType (f t) as


-- | Remove annotaions form type list
clearAnn :: [AnnotatedType] -> [Type]
clearAnn = map toType


isRef :: AnnotatedType -> Bool
isRef (AnnotatedType _ as) = Ref `elem` as

-- = Templates

-- | List of all template arguments
getPlaceholders :: Type -> [String]
getPlaceholders (Placeholder s) = [s]
getPlaceholders (StructType _ ts) =
    concatMap (getPlaceholders . snd) ts
getPlaceholders (ProcType ts) =
    concatMap (getPlaceholders . toType) ts
getPlaceholders (FuncType ts ts') =
    concatMap (getPlaceholders) ts ++
        concatMap (getPlaceholders . toType) ts'
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
        instann = map (mapAnn instf) 
     in case t of
          (Placeholder s) -> f s
          (ProcType ts) -> ProcType (instann ts)
          (FuncType ts ts') -> FuncType (instmap ts) (instann ts')
          (StructType struct ts) ->
              let names = map fst ts
                  types = map (instf . snd) ts
               in StructType struct (zip names types)
          tt -> tt


-- = Compatibility
-- Two types are compatible if they are equivalent
-- with respect to parameter passing

-- | Tests if two types are compatible
compatible :: Type -> Type -> Bool
compatible (NamedType n1) (StructType n2 _) = n1 == n2
compatible (StructType n1 _) (NamedType n2) = n1 == n2
compatible (ProcType tsAnn1) (ProcType tsAnn2) =
     and $ zipWith compatible ts1 ts2
         where ts1 = clearAnn tsAnn1
               ts2 = clearAnn tsAnn2
compatible (FuncType rts1 atsAnn1) (FuncType rts2 atsAnn2) =
    let ats1 = clearAnn atsAnn1
        ats2 = clearAnn atsAnn2
        comp2 = zipWith compatible
     in and (comp2 rts1 rts2) && and (comp2 ats1 ats2)
compatible t1 t2 = t1 == t2


-- = Similarity
-- Two types are similar if it is not possible
-- to exist overloaded objects with both signatures,
-- that is, the interpreter can distinguish them using
-- available contextual information.

-- | Check if types are similar
similar :: Type -> Type -> Bool
similar (ProcType tsAnn1) (ProcType tsAnn2) =
    let ts1 = clearAnn tsAnn1
        ts2 = clearAnn tsAnn2
     in and $ zipWith compatible ts1 ts2
similar (FuncType _ atsAnn1) (FuncType _ atsAnn2) =
    let ats1 = clearAnn atsAnn1
        ats2 = clearAnn atsAnn2
     in and $ zipWith compatible ats1 ats2
similar _ _ = False


