module Exec.Interpreter where

import Exec.Prim
import Exec.Expr
import Syntactic
import PosParsec


-- = Statements

-- | Conditional execution

-- | Auxiliar function to allow folding
runIfPart :: Exec Bool 
          -> (Located SynExpr, Located SynBlock)
          -> Exec Bool
runIfPart val (lexpr, lblock) =
    do other <- val
       if other == True
       then return True
       else do c <- evalExpr lexpr
               if c == BoolVal True
               then do raiseScope
                       runBlock lblock
                       dropScope
                       return True
               else return False


-- | if/else if/else structure execution
runIf :: (Located SynIf) -> Exec ()
runIf locif =
    let ifx = ignorepos locif
    in case ifx of
         (SynIf xs xelse) ->
             do done <- foldl runIfPart (return False) xs
                if not done
                then case xelse of                      
                       Just xblock ->
                           do raiseScope
                              runBlock xblock
                              dropScope
                       Nothing -> return ()
                else return ()


-- | while block execution
runWhile :: (Located SynWhile) -> Exec ()
runWhile locwhile =
    let while = ignorepos locwhile
        runRecWhile cond block = 
            do v <- evalExpr cond
               if v == BoolVal True
               then runBlock block >> runRecWhile cond block
               else return ()
    in do
        raiseScope
        runRecWhile (getWhileCondition while) (getWhileBlock while)
        dropScope


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
         (SynStmtIf locif) -> runIf locif
         (SynStmtWhile locwhile) -> runWhile locwhile
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
