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
data Exec a = Exec { execIO :: ProgramState -> IO ProgramState
                   , evalIO :: ProgramState -> IO a }

-- | Create valueless execution part
mkExec :: (ProgramState -> IO ProgramState)
       -> Exec ()
mkExec f = Exec f (const $ return ())

-- | Create evaluation execution part with no side effects
mkEval :: (ProgramState -> IO a)
       -> Exec a
mkEval f = Exec return f

execmap :: (a -> b) -> Exec a -> Exec b
execmap f (Exec x v) = Exec x (fmap f . v) 

execunit :: a -> Exec a
execunit x = Exec return (const $ return x)

execbind :: Exec a -> (a -> Exec b) -> Exec b
execbind m k = Exec execS evalS
    where execS s1 = do val <- evalIO m s1
                        s2  <- execIO m s1
                        execIO (k val) s2

          evalS s1 = do val <- evalIO m s1
                        s2  <- execIO m s1
                        evalIO (k val) s2

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

findproc :: String -> Exec (SynProc)
findproc procname =
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

runStmt :: (Located SynStmt) -> Exec ()
runStmt locstmt =
    case stmt of
      (SynStmtDef locdef) -> runDef locdef
      _ -> return ()
    where stmt = ignorepos locstmt 

runBlock :: (Located SynBlock) -> Exec ()
runBlock locb = mapM_ runStmt $ getStmts b
    where b = ignorepos locb

runProc :: SynProc -> Exec ()
runProc p = do runPrintLn $ "starting procedure " ++ getProcName p
               runStatus
               runBlock $ getProcBlock p
               runPrintLn $ "ending procedure " ++ getProcName p
               runStatus

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
       main <- findproc "main"
       runProc main 
