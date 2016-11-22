module Exec.Prim where

import Control.Monad
import Data.List (find)
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
         | StructVal Type [Val]
         | None
         deriving (Eq)


instance Show Val where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (BoolVal b) = if b then "true" else "false"
    show (StructVal (StructType n ts) vals) =
        n ++ " {" ++ show vals ++ "}"
        

instance Typed Val where
    toType (IntVal _) = IntType
    toType (FloatVal _) = FloatType
    toType (BoolVal _) = BoolType
    toType None = NoneType


-- | Variable
data Var = Var { getVarName :: String 
               , getVarType :: Type
               , getVarValue :: Val
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


-- | Update variable value
setVarValue :: Var -> Val -> Var
setVarValue var val =
    Var (getVarName var) (getVarType var) val (getVarScope var)


-- | Update variable scope
setVarScope :: Var -> Scope -> Var
setVarScope var scope =
    Var (getVarName var) (getVarType var) (getVarValue var) scope


type StructTable = [Type]
type ProcTable = [Proc]
type FuncTable = [Func]

-- | Program State
data ProgramState =
    State { getVarTable :: [Var] -- ^ Variable table
          , getStructTable :: StructTable -- ^ Struct table
          , getProcTable :: ProcTable -- ^ Procedure table
          , getFuncTable :: FuncTable -- ^ Function table
          }


-- | Initial program state
newProgramState :: ProgramState
newProgramState = State [] [] [] []


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
               in return (State vt st pt ft)


-- | Get current struct table
obtainStructTable :: Exec StructTable
obtainStructTable = mkEval $ return . getStructTable


-- | Set current procedure table
modifyStructTable :: StructTable -> Exec ()
modifyStructTable st = mkExec $
    \state -> let vt = getVarTable state
                  pt = getProcTable state
                  ft = getFuncTable state
               in return (State vt st pt ft)


-- | Get current procedure table
obtainProcTable :: Exec ProcTable
obtainProcTable = mkEval $ return . getProcTable


-- | Set current procedure table
modifyProcTable :: ProcTable -> Exec ()
modifyProcTable pt = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  ft = getFuncTable state
               in return (State vt st pt ft)


-- | Get current function table
obtainFuncTable :: Exec FuncTable
obtainFuncTable = mkEval $ return . getFuncTable


-- | Set current procedure table
modifyFuncTable :: FuncTable -> Exec ()
modifyFuncTable ft = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  pt = getProcTable state
               in return (State vt st pt ft)



-- = Auxiliary functions

-- == Debug

-- | Print line
runPrintLn :: String -> Exec ()
runPrintLn s = mkExec $ \state ->
    do putStrLn s
       return state

-- | Prints full variable table
runStatus :: Exec ()
runStatus =
    do runPrintLn "status> "
       vt <- obtainVarTable
       let pvar var = getName var ++ "=" ++ show (getVarValue var) ++ ", "
       runPrintLn $ "vars: " ++ (concatMap pvar vt)
       -- st <- obtainStructTable
       -- runPrintLn $ "structs:" ++ (map show st)

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
           matchtype = (==proctype) . toType
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
           matchtype = (funcSim $ FuncType [] argtypes) . toType
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
findType (SynType locident)
  | n == "int" = return IntType
  | n == "float" = return FloatType
  | n == "bool" = return BoolType
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
       modifyVarTable $ Var vname vtype vvalue (Local 0) : vt 


-- | Define local variable with no value
registerLocalUndefVar :: String -> Type -> Exec ()
registerLocalUndefVar vname vtype =
    registerLocalVar vname vtype None 


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


-- | Change variable value by name
changeVar :: String -> Val -> Exec ()
changeVar vname val =
    do vt <- obtainVarTable
       let vt' = updateWhen ((==vname) . getVarName) vt val
       modifyVarTable vt'
    where 
        updateWhen _ [] _ = []
        updateWhen cond (v:vs) value
          | cond v = setVarValue v value : vs
          | otherwise = v : updateWhen cond vs value


