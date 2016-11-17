module Exec.Interpreter where

import Control.Monad (ap)
import Data.List (find)
import Syntactic
import PosParsec

-- = Program State

-- | Scope level
data Scope = Global 
           | Local Int


-- | Value
data Val = IntVal Int
         | FloatVal Float
         | BoolVal Bool
         | None
         deriving (Show)


-- | Value type
data Type = IntType
          | FloatType
          | BoolType
          | StructType String [(String, Type)]
          | ProcType [Type]
          | FuncType [Type] [Type] deriving (Show)


-- | Variable
data Var = Var { getVarName :: String 
               , getVarType :: Type
               , getVarValue :: Val
               } deriving (Show)


-- | Update variable value
setVarValue :: Var -> Val -> Var
setVarValue var val = Var (getVarName var) (getVarType var) val


-- | Program State
data ProgramState =
    State { getVarTable :: [Var] -- ^ Variable table
          , getStructTable :: [Type] -- ^ Struct table
          , getProcTable :: [SynProc] -- ^ Procedure table
          , getFuncTable :: [SynFunc] -- ^ Function table
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
obtainStructTable :: Exec [Type]
obtainStructTable = mkEval $ return . getStructTable


-- | Get current procedure table
obtainProcTable :: Exec [SynProc]
obtainProcTable = mkEval $ return . getProcTable


-- | Set current procedure table
modifyProcTable :: [SynProc] -> Exec ()
modifyProcTable pt = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  ft = getFuncTable state
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
    do vt <- obtainVarTable
       runPrintLn "status> "
       mapM_ (runPrintLn . show) vt


-- == Procedure table auxiliary functions

findProc :: String -> Exec SynProc
findProc procname =
    do procs <- obtainProcTable
       let proc = find (\p -> getProcName p == procname) procs
       case proc of
         Just p -> return p
         Nothing -> error $ "couldn't find procedure " ++ procname


-- == Struct table auxiliary functions

findType :: SynIdent -> Exec Type
findType (SynIdent i)
  | i == "int" = return IntType
  | i == "float" = return FloatType
  | i == "bool" = return BoolType
  | otherwise = error $ "couldn't find type " ++ i


-- == Variable table auxiliary functions

-- | Search for variable on table
findVar :: SynIdent -> Exec Var
findVar varident =
    do vars <- obtainVarTable
       let varname = getlabel varident
           var = find ((== varname) . getVarName) vars
       case var of
         Just p -> return p
         Nothing -> error $ "variable not found: " ++ varname


-- | Define local variable
registerLocalVar :: String -> Type -> Val -> Exec ()
registerLocalVar vname vtype vvalue =
    do vt <- obtainVarTable
       modifyVarTable $ Var vname vtype vvalue : vt 


-- | Define local variable with no value
registerLocalUndefVar :: String -> Type -> Exec ()
registerLocalUndefVar vname vtype =
    registerLocalVar vname vtype None


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


-- = Expressions

-- | Execution to add two values
addVal :: Val -> Val -> Exec Val
addVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 + i2
addVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 + f2
addVal _ _ = return None


-- | Execution to multiply two values
timesVal :: Val -> Val -> Exec Val
timesVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 * i2
timesVal _ _ = return None


-- | Execution to 'not' a value
notVal :: Val -> Exec Val
notVal (BoolVal b) = return $ BoolVal $ not b
notVal _ = return None


-- | Execution to 'and' two values
andVal :: Val -> Val -> Exec Val
andVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 && b2
andVal _ _ = return None


-- | Execution to 'or' two values
orVal :: Val -> Val -> Exec Val
orVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 || b2
orVal _ _ = return None


-- | Execution to 'xor' two values
xorVal :: Val -> Val -> Exec Val
xorVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ (b1 && not b2) || (not b1 && b2)
xorVal _ _ = return None


-- | Execution to run binary operation
evalUn :: (Val -> Exec Val) -- ^ Value computation function
       -> (Located SynExpr) -- ^ Expression
       -> Exec Val
evalUn f e =
    do v <- evalExpr e
       f v


-- | Execution to run binary operation
evalBin :: (Val -> Val -> Exec Val) -- ^ Value computation function
        -> (Located SynExpr) -- ^ Left expression
        -> (Located SynExpr) -- ^ Right expression
        -> Exec Val
evalBin f e1 e2 =
    do v1 <- evalExpr e1
       v2 <- evalExpr e2
       f v1 v2


-- | Execution to evaluate expression
evalExpr :: (Located SynExpr) -> Exec Val
evalExpr = eval . ignorepos
    where eval :: SynExpr -> Exec Val

          eval (SynLitIntExpr locint) =
              return $ IntVal $ getint . ignorepos $ locint

          eval (SynLitBoolExpr locbool) =
              return $ BoolVal $ getbool . ignorepos $ locbool

          eval (SynIdentExpr locident) =
              do var <- findVar $ ignorepos locident
                 return $ getVarValue var

          eval (SynPar e) = evalExpr e

          eval (SynPlus e1 e2) = evalBin addVal e1 e2

          eval (SynTimes e1 e2) = evalBin timesVal e1 e2

          eval (SynNot e) = evalUn notVal e

          eval (SynAnd e1 e2) = evalBin andVal e1 e2

          eval (SynOr e1 e2) = evalBin orVal e1 e2

          eval (SynXor e1 e2) = evalBin xorVal e1 e2


-- = Statements

-- | Definition execution
runDef :: (Located SynDef) -> Exec ()
runDef locdef = 
    let tpIdents = getDefTypedIdents . ignorepos $ locdef
        regvar tpIdent =
            do tp <- findType $ ignorepos . getTypedIdentType $ tpIdent
               let name = getlabel . ignorepos . getTypedIdentName $ tpIdent
               registerLocalUndefVar name tp
    in mapM_ regvar tpIdents


-- | Attribution execution
runAttr :: (Located SynAttr) -> Exec ()
runAttr locattr =
    do let attr = ignorepos locattr
           locidents = getAttrVars attr 
           locexprs = getAttrExprs attr
       runPrintLn $ "attr for " ++ show locidents
       vals <- mapM evalExpr locexprs
       let names = map (getlabel . ignorepos) locidents
       sequence_ $ zipWith changeVar names vals
       runStatus


-- | Statement execution
runStmt :: (Located SynStmt) -> Exec ()
runStmt locstmt =
    do let stmt = ignorepos locstmt
       case stmt of
         (SynStmtDef locdef) -> runDef locdef
         (SynStmtAttr locattr) -> runAttr locattr
         _ -> return ()


-- | Block execution
runBlock :: (Located SynBlock) -> Exec ()
runBlock locblock =
    do let block = ignorepos locblock
       mapM_ runStmt $ getStmts block


-- = Procedures

-- | Procedure execution
runProc :: SynProc -> Exec ()
runProc p = do runPrintLn $ "starting procedure " ++ getProcName p
               runBlock $ getProcBlock p
               runPrintLn $ "ending procedure " ++ getProcName p


-- = Module Execution

-- | Register procedure into procedure table
registerProc :: SynProc -> Exec ()
registerProc p =
    do procs <- obtainProcTable
       modifyProcTable $ p : procs


-- | Load global variables, procedures, functions ans structs
loadModuleSymbols :: SynModule -> Exec ()
loadModuleSymbols mod = mapM_ loadSymbol (modStmts mod)
    where
        loadSymbol (SynModDef locdef) = return ()
        loadSymbol (SynModProc locproc) =
            do let proc = ignorepos locproc
               pt <- obtainProcTable
               modifyProcTable $ proc : pt
        loadSymbol (SynModFunc locfunc) = return ()
        loadSymbol (SynModStruct locstruct) = return ()
        

-- | Module execution
runmodule :: SynModule -> Exec ()
runmodule m =
    do loadModuleSymbols m
       runPrintLn "Hello"
       main <- findProc "main"
       runProc main 
