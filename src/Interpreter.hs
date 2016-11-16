module Interpreter where

import Control.Monad (ap)
import Data.List (find)
import Syntactic
import PosParsec

data Scope = Global | Local Int

data Val = IntVal Int | FloatVal Float | BoolVal Bool | None deriving (Show)

data Type = IntType
          | FloatType
          | BoolType
          | StructType String [(String, Type)]
          | ProcType [Type]
          | FuncType [Type] [Type] deriving (Show)


data Var = Var { getVarName :: String 
               , getVarType :: Type
               , getVarValue :: Val
               } deriving (Show)

setVarValue :: Var -> Val -> Var
setVarValue var val = Var (getVarName var) (getVarType var) val

data ProgramState = State { getVarTable :: [Var] -- ^ Variable table
                          , getStructTable :: [Type] -- ^ Struct table
                          , getProcTable :: [SynProc] -- ^ Procedure table
                          , getFuncTable :: [SynFunc] -- ^ Function table
                          }

newProgramState :: ProgramState
newProgramState = State [] [] [] []
-----------------------------------------------------------------

-- | Execution part type
-- Contains a state updating function and
-- a value obtention function
data Exec a =
    Exec { execEvalIO :: ProgramState -> IO (ProgramState, a) }

execIO :: Exec a -> ProgramState -> IO ProgramState
execIO ex state = fmap fst $ execEvalIO ex state

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


execmap :: (a -> b) -> Exec a -> Exec b
execmap f (Exec x) = Exec $
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
    fmap = execmap

instance Applicative Exec where
    pure = execunit
    (<*>) = ap

instance Monad Exec where
    return = execunit
    (>>=) = execbind

----------------------------------------------------

obtainVarTable :: Exec [Var]
obtainVarTable = mkEval $ return . getVarTable

modifyVarTable :: [Var] -> Exec ()
modifyVarTable vt = mkExec $
    \state -> let st = getStructTable state
                  pt = getProcTable state
                  ft = getFuncTable state
               in return (State vt st pt ft)

obtainStructTable :: Exec [Type]
obtainStructTable = mkEval $ return . getStructTable

obtainProcTable :: Exec [SynProc]
obtainProcTable = mkEval $ return . getProcTable

modifyProcTable :: [SynProc] -> Exec ()
modifyProcTable pt = mkExec $
    \state -> let vt = getVarTable state
                  st = getStructTable state
                  ft = getFuncTable state
               in return (State vt st pt ft)

----------------------------------------------------

runPrintLn :: String -> Exec ()
runPrintLn s = mkExec $ \state ->
    do putStrLn s
       return state


runStatus :: Exec ()
runStatus =
    do vt <- obtainVarTable
       runPrintLn "status> "
       mapM_ (runPrintLn . show) vt

----------------------------------------------------

findProc :: String -> Exec SynProc
findProc procname =
    do procs <- obtainProcTable
       let proc = find (\p -> getProcName p == procname) procs
       case proc of
         Just p -> return p
         Nothing -> error $ "couldn't find procedure " ++ procname


findType :: SynIdent -> Exec Type
findType (SynIdent i)
  | i == "int" = return IntType
  | i == "float" = return FloatType
  | i == "bool" = return BoolType
  | otherwise = error $ "couldn't find type " ++ i


registerLocalVar :: String -> Type -> Val -> Exec ()
registerLocalVar vname vtype vvalue =
    do vt <- obtainVarTable
       modifyVarTable $ Var vname vtype vvalue : vt 


registerLocalUndefVar :: String -> Type -> Exec ()
registerLocalUndefVar vname vtype =
    registerLocalVar vname vtype None


runDef :: (Located SynDef) -> Exec ()
runDef locdef = 
    let tpIdents = getDefTypedIdents . ignorepos $ locdef
        regvar tpIdent =
            do tp <- findType $ ignorepos . getTypedIdentType $ tpIdent
               let name = getlabel . ignorepos . getTypedIdentName $ tpIdent
               registerLocalUndefVar name tp
    in mapM_ regvar tpIdents


evalExpr :: (Located SynExpr) -> Exec Val
evalExpr locexpr = return $ IntVal 0


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

runStmt :: (Located SynStmt) -> Exec ()
runStmt locstmt =
    do let stmt = ignorepos locstmt
       case stmt of
         (SynStmtDef locdef) -> runDef locdef
         (SynStmtAttr locattr) -> runAttr locattr
         _ -> return ()

runBlock :: (Located SynBlock) -> Exec ()
runBlock locblock =
    do let block = ignorepos locblock
       mapM_ runStmt $ getStmts block

runProc :: SynProc -> Exec ()
runProc p = do runPrintLn $ "starting procedure " ++ getProcName p
               --runStatus
               runBlock $ getProcBlock p
               runPrintLn $ "ending procedure " ++ getProcName p
               --runStatus

-------------------------------------------------

registerProc :: SynProc -> Exec ()
registerProc p =
    do procs <- obtainProcTable
       modifyProcTable $ p : procs

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
        

runmodule :: SynModule -> Exec ()
runmodule m =
    do loadModuleSymbols m
       runPrintLn "Hello"
       main <- findProc "main"
       runProc main 
