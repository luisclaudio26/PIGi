module Exec.Prim where

import Control.Monad
import Data.List (find, elemIndex)
import Data.Maybe (fromJust)
import Syntactic
import PosParsec
import Types

-- = Program State

-- | Scope level
data Scope = Global 
           | Local Int
           deriving (Show, Eq)


-- | Value
data Val = IntVal Int
         | FloatVal Float
         | BoolVal Bool
         | StrVal String
         | StructVal Type [Val]
         | MatVal Int Int [[Val]]
         | None
         deriving (Eq)


instance Show Val where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (BoolVal b) = if b then "true" else "false"
    show (StructVal (StructType n ts) vals) =
        n ++ " {" ++ show vals ++ "}"
    show (MatVal _ _ mat) = show mat
    show None = "NONE"
        

instance Typed Val where
    toType (IntVal _) = IntType
    toType (FloatVal _) = FloatType
    toType (BoolVal _) = BoolType
    toType (StrVal _) = StrType
    toType (StructVal t _) = t
    toType (MatVal _ _ _) = MatType
    toType None = NoneType


-- == Memory

type MemLoc = Int
type RefCount = Int
data Access = Field Name | Index [Int] deriving (Show)
data AccessPath = AccessPath { memloc ::  MemLoc
                             , access :: [Access]
                             } deriving (Show)

-- | Access path to memory location
pureLoc :: MemLoc -> AccessPath
pureLoc loc = AccessPath loc []


-- | Append access instructions to access path
appendAccess :: AccessPath -> [Access] -> AccessPath
appendAccess (AccessPath loc acc) acc' = AccessPath loc $ acc ++ acc'


-- | Data Object
data Obj = Obj { getObjAddr :: MemLoc
               , getObjRefCount :: RefCount
               , getObjValue :: Val
               }

setObjValue :: Obj -> Val -> Obj
setObjValue (Obj addr rc val) val' = Obj addr rc val'


-- | Variable
data Var = Var { getVarName :: String 
               , getVarType :: Type
               , getVarAddr :: AccessPath
               , getVarScope :: Scope
               } deriving (Show)


instance Typed Var where
    toType = getVarType

instance Named Var where
    getName = getVarName

-- | PIG Procedure
data Proc = NativeProc Name Type ([Val] -> Exec ())
          | Proc SynProc

instance Typed Proc where
    toType (NativeProc _ t _) = t
    toType (Proc sproc) = toType sproc

instance Named Proc where
    getName (NativeProc n _ _) = n
    getName (Proc sproc) = getName sproc


-- | PIG Function
data Func = NativeFunc Name Type ([Val] -> Exec [Val])
          | Func SynFunc

instance Typed Func where
    toType (NativeFunc _ t _) = t
    toType (Func sfunc) = toType sfunc

instance Named Func where
    getName (NativeFunc n _ _) = n
    getName (Func sfunc) = getName sfunc


-- | Update variable scope
setVarScope :: Var -> Scope -> Var
setVarScope var scope =
    Var (getVarName var) (getVarType var) (getVarAddr var) scope


type VarTable = [Var]
type StructTable = [Type]
type ProcTable = [Proc]
type FuncTable = [Func]

data MemTable = MemTable { getNextAddr :: MemLoc
                         , getMemObjs :: [Obj]
                         }

-- | Program State
data ProgramState =
    State { getVarTable :: [Var] -- ^ Variable table
          , getStructTable :: StructTable -- ^ Struct table
          , getProcTable :: ProcTable -- ^ Procedure table
          , getFuncTable :: FuncTable -- ^ Function table
          , getMemTable :: MemTable -- ^ Memory objects
          }


-- | Initial program state
newProgramState :: ProgramState
newProgramState = State [] [] [] [] (MemTable 0 [])


-- = Execution monad

-- | Execution monad
-- Contains a state updating function and
-- a value obtention function
data Exec a =
    Exec { execEvalIO :: ProgramState -> IO (ProgramState, a) }


-- | Run execution, ignore value
execIO :: Exec a -> ProgramState -> IO ProgramState
execIO ex state = fmap fst $ execEvalIO ex state


-- | Run execution, ignore new state
evalIO :: Exec a -> ProgramState -> IO a
evalIO ex state = fmap snd $ execEvalIO ex state


-- | Create valueless execution part
mkExec :: (ProgramState -> IO ProgramState)
       -> Exec ()
mkExec f = Exec $ \state ->
    do state' <- f state
       return (state', ())


-- | Create evaluation execution part with no side effects
mkEval :: (ProgramState -> IO a)
       -> Exec a
mkEval f = Exec $ \state ->
    do val <- f state
       return (state, val)


execfmap :: (a -> b) -> Exec a -> Exec b
execfmap f (Exec x) = Exec $
    \state -> do evalexec <- x state
                 let state' = fst evalexec
                     val = snd evalexec
                 return (state', f val)


execunit :: a -> Exec a
execunit u = Exec $
    \state -> return (state, u)


execbind :: Exec a -> (a -> Exec b) -> Exec b
execbind m k = Exec $
    \state -> do mid <- execEvalIO m state
                 let state' = fst mid
                     val = snd mid
                 execEvalIO (k val) state'


instance Functor Exec where
    fmap = execfmap


instance Applicative Exec where
    pure = execunit
    (<*>) = ap


instance Monad Exec where
    return = execunit
    (>>=) = execbind


-- = Table access execution

-- | Get current variable table
obtainVarTable :: Exec [Var]
obtainVarTable = mkEval $ return . getVarTable


-- | Set current variable table
modifyVarTable :: [Var] -> Exec ()
modifyVarTable vt = mkExec $
    \state -> let st = getStructTable state
                  pt = getProcTable state
                  ft = getFuncTable state
                  mt = getMemTable state
               in return (State vt st pt ft mt)


-- | Get current struct table
obtainStructTable :: Exec StructTable
obtainStructTable = mkEval $ return . getStructTable


-- | Set current procedure table
modifyStructTable :: StructTable -> Exec ()
modifyStructTable st = mkExec $
    \state -> let vt = getVarTable state
                  pt = getProcTable state
                  ft = getFuncTable state
                  mt = getMemTable state
               in return (State vt st pt ft mt)


-- | Get current procedure table
obtainProcTable :: Exec ProcTable
obtainProcTable = mkEval $ return . getProcTable


-- | Set current procedure table
modifyProcTable :: ProcTable -> Exec ()
modifyProcTable pt = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  ft = getFuncTable state
                  mt = getMemTable state
               in return (State vt st pt ft mt)


-- | Get current function table
obtainFuncTable :: Exec FuncTable
obtainFuncTable = mkEval $ return . getFuncTable


-- | Set current procedure table
modifyFuncTable :: FuncTable -> Exec ()
modifyFuncTable ft = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  pt = getProcTable state
                  mt = getMemTable state
               in return (State vt st pt ft mt)

-- | Get current memory table
obtainMemTable :: Exec MemTable
obtainMemTable = mkEval $ return . getMemTable


-- | Set current memory table
modifyMemTable :: MemTable -> Exec ()
modifyMemTable mt = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  pt = getProcTable state
                  ft = getFuncTable state
               in return (State vt st pt ft mt)


-- = Auxiliary functions

-- == Struct table auxiliary functions

findStruct :: String -> Exec Type
findStruct structname =
    do structs <- obtainStructTable
       let matchstruct (StructType n _) = n == structname
           matchstruct _ = False
           struct = find matchstruct structs
       case struct of
         Just st -> return st
         Nothing -> error $ "couldn't find struct " ++ structname
   

-- | Register struct into strcut table
registerStruct :: Type -> Exec ()
registerStruct st@(StructType _ _) =
    do structs <- obtainStructTable
       modifyStructTable $ st : structs
registerStruct _ = error "can't register non-struct"


-- == Procedure table auxiliary functions

findProc :: String -> Type -> Exec Proc
findProc procname proctype =
    do procs <- obtainProcTable
       let matchname = (==procname) . getName
           matchtype = (similar proctype) . toType
           proc = find (\s -> matchname s && matchtype s) procs
       case proc of
         Just p -> return p
         Nothing -> error $ "couldn't find procedure " ++ procname


-- | Register procedure into procedure table
registerProc :: Proc -> Exec ()
registerProc p =
    do procs <- obtainProcTable
       modifyProcTable $ p : procs


-- == Function table auxiliary functions

findFunc :: String -> [Type] -> Exec Func
findFunc funcname argtypes =
    do funcs <- obtainFuncTable
       let matchname = (==funcname) . getName
           anntypes = map toAnnType argtypes
           matchtype = (similar $ FuncType [] anntypes) . toType
           func = find (\f -> matchname f && matchtype f) funcs
       case func of
         Just d -> return d
         Nothing -> error $ "couldn't find function " ++ funcname


-- | Register function into function table
registerFunc :: Func -> Exec ()
registerFunc f =
    do funcs <- obtainFuncTable
       modifyFuncTable $ f : funcs


-- == Struct table auxiliary functions

findType :: SynType -> Exec Type

findType (SynTypeNGen _ locident)
  | n == "int" = return IntType
  | n == "float" = return FloatType
  | n == "bool" = return BoolType
  | n == "string" = return StrType
  | n == "mat" = return MatType
  | otherwise = return $ NamedType n
  where n = getName locident

-- == Variable table auxiliary functions

-- | Search for variable on table
findVar :: String -> Exec Var
findVar varname =
    do vars <- obtainVarTable
       let var = find ((== varname) . getVarName) vars
       case var of
         Just p -> return p
         Nothing -> error $ "variable not found: " ++ varname


-- | Define local variable
registerLocalVar :: String -> Type -> Val -> Exec ()
registerLocalVar vname vtype vvalue =
    do vt <- obtainVarTable
       loc <- addObj vvalue
       let addr = pureLoc loc
       modifyVarTable $ Var vname vtype addr (Local 0) : vt 


-- | Define local variable with no value
registerLocalUndefVar :: String -> Type -> Exec ()
registerLocalUndefVar vname vtype =
    registerLocalVar vname vtype None


-- | Define local reference
registerLocalRef :: String -> Type -> AccessPath -> Exec ()
registerLocalRef vname vtype vaddr =
    do vt <- obtainVarTable
       modifyVarTable $ Var vname vtype vaddr (Local 0) : vt

 
-- | Get struct field value
getStructField :: Val -> Name -> Val
getStructField (StructVal tp vals) fieldname = vals !! idx
    where idx = getFieldIndex tp fieldname


-- | Set struct field value
setStructField :: Val -> Name -> Val -> Val
setStructField (StructVal tp vals) fieldname val' = StructVal tp vals'
    where idx = getFieldIndex tp fieldname
          bef = take idx vals
          aft = drop (idx+1) vals
          vals' = bef ++ [val'] ++ aft


-- | Get matrix value at indices
getMatVal :: Val -> [Int] -> Val
getMatVal (MatVal _ _ mat) [i, j] = (mat !! i) !! j
getMatVal (MatVal m n mat) [i] = (mat !! row) !! col
    where row = div i n
          col = mod i n


-- | Set matrix value at indices 
setMatVal :: Val -> [Int] -> Val -> Val
setMatVal (MatVal m n mat) idxs val' = MatVal m n mat'
    where row = case idxs of
                  [i, _] -> i
                  [i] -> div i n
          col = case idxs of
                  [_, j] -> j
                  [i] -> mod i n
          rowbef = take col (mat !! row)
          rowaft = drop (col+1) (mat !! row)
          row' = rowbef ++ [val'] ++ rowaft
          matbef = take row mat
          mataft = drop (row+1) mat
          mat' = matbef ++ [row'] ++ mataft


-- | Get value by access path
getValAccess :: Val -> [Access] -> Val
getValAccess val [] = val
getValAccess matval [Index is] = getMatVal matval is
getValAccess structval (Field s:as) =
    getValAccess (getStructField structval s) as


-- | Set value by access path
setValAccess :: Val -> [Access] -> Val -> Val
setValAccess val [] val' = val'
setValAccess mat [Index is] val' = setMatVal mat is val'
setValAccess st (Field field:as) val' =
    setStructField st field (setValAccess fval as val')
    where fval = getStructField st  field


-- | Obtain variable value
obtainVarValue :: String -> Exec Val
obtainVarValue vname =
    do var <- findVar vname
       let addr = getVarAddr var
           loc = memloc addr
           path = access addr
       obj <- findObj loc
       let val = getObjValue obj
       return $ getValAccess val path


-- | Modify variable value
modifyVarValue :: String -> Val -> Exec ()
modifyVarValue vname val =
    do var <- findVar vname
       let addr = getVarAddr var
       setValAt val addr


-- | Obtain LValue value
obtainLValue :: Name -> [Access] -> Exec Val
obtainLValue vname path =
    do val <- obtainVarValue vname
       return $ getValAccess val path


-- | Modify LValue value
modifyLValue :: Name -> [Access] -> Val -> Exec ()
modifyLValue vname path fval =
    do val <- obtainVarValue vname
       let val' = setValAccess val path fval
       modifyVarValue vname val'


-- | Increment scope level for local variables
raiseScope :: Exec ()
raiseScope =
    do vt <- obtainVarTable
       modifyVarTable $ foldl raise [] vt
    where
        raise vt var =
            case getVarScope var of
              (Local n) -> setVarScope var (Local $ n+1) : vt
              _ -> var:vt


-- | Decrement scope level for local variables
dropScope :: Exec ()
dropScope =
    do vt <- obtainVarTable
       modifyVarTable $ foldl drop [] vt
    where
        drop vt var =
            case getVarScope var of
              (Local 0) -> vt
              (Local n) -> setVarScope var (Local $ n-1) : vt
              _ -> var:vt


-- | Return scope and clear local variables
saveAndClearScope :: Exec [Var]
saveAndClearScope =
    do vt <- obtainVarTable
       modifyVarTable $ filter ((== Global) . getVarScope) vt
       return vt


-- == Memory table auxiliary functions

findObj :: MemLoc -> Exec Obj
findObj addr =
    do mem <- obtainMemTable
       let obj = find ((==addr) . getObjAddr) (getMemObjs mem)
       case obj of
         Just obj' -> return obj'
         Nothing -> error $ "object at " ++ show addr ++ " not found"


setValAt :: Val -> AccessPath -> Exec ()
setValAt val addr =
    do mem <- obtainMemTable
       let changeObj obj =
               if getObjAddr obj == memloc addr
                  then setObjValue obj val
                  else obj
           newObjs = map changeObj (getMemObjs mem)
       modifyMemTable $ MemTable (getNextAddr mem) newObjs


addObj :: Val -> Exec MemLoc
addObj val =
    do mem <- obtainMemTable
       let addr = getNextAddr mem
           objs = (Obj addr 1 val) : getMemObjs mem
           nextAddr = addr + 1
       modifyMemTable $ MemTable nextAddr objs
       return addr
