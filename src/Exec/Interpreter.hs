module Exec.Interpreter where

import Control.Monad
import Exec.Prim
import Exec.Expr
import Exec.Native
import Types
import Syntactic
import PosParsec
import Data.List (elemIndex)


-- = Expressions
-- These functions are not part of Exec.Expr to
-- avoid circular imports

-- | Execution to run binary operation
evalUn :: (Val -> Exec Val) -- ^ Value computation function
       -> (Located SynExpr) -- ^ Expression
       -> Exec Val
evalUn comp expr =
    do val <- evalExpr expr
       comp val


-- | Execution to run binary operation
evalBin :: (Val -> Val -> Exec Val) -- ^ Value computation function
        -> (Located SynExpr) -- ^ Left expression
        -> (Located SynExpr) -- ^ Right expression
        -> Exec Val
evalBin comp expr1 expr2 =
    do v1 <- evalExpr expr1
       v2 <- evalExpr expr2
       comp v1 v2


-- | Execution of struct value extraction
obtStructVal :: LocSynExpr -> Located SynIdent -> Exec Val
obtStructVal locstexpr locident =
    do let stexpr = ignorepos locstexpr
           ident  = ignorepos locident
           extrNames (StructType _ ts) = map fst ts
           idname = getName locident
       stval <- evalExpr locstexpr
       case stval of
         (StructVal sttype vals) ->
             let ns = extrNames sttype
                 idx = elemIndex idname ns
                 in case idx of
                      Just i -> return $ vals !! i
                      Nothing -> error "field not found"
         _ -> error "can't access non-structure"


-- | Execution of matrix value extraction
obtMatVal :: [[LocSynExpr]] -> Exec Val
obtMatVal mat =
    do vals <- mapM (mapM evalExpr) mat
       let rows = length vals
           cols = length $ head vals
        in return $ MatVal rows cols vals


-- | Execution of indexing
obtIndexVal :: LocSynExpr -> [LocSynExpr] -> Exec Val
obtIndexVal matexpr idxexprs =
    do matval <- evalExpr matexpr
       idxs <- mapM evalExpr idxexprs
       case matval of
         (MatVal m n mat) ->
             case idxs of
               [IntVal i, IntVal j] -> return ((mat !! i) !! j)
               _ -> error "invalid indexing"
         _ -> error "invalid indexed"
       


-- | Execution to evaluate expression
evalExpr :: (Located SynExpr) -> Exec Val
evalExpr = eval . ignorepos
    where eval :: SynExpr -> Exec Val

          eval (SynLitIntExpr locint) =
              return $ IntVal $ getint . ignorepos $ locint

          eval (SynLitFloatExpr locfloat) =
              return . FloatVal . getfloat . ignorepos $ locfloat

          eval (SynLitBoolExpr locbool) =
              return $ BoolVal $ getbool . ignorepos $ locbool

          eval (SynLitStrExpr locstr) =
              return $ StrVal $ getstr . ignorepos $ locstr

          eval (SynIdentExpr locident) =
              do obtainVarValue $ getName locident

          eval (SynCallExpr loccall) = fmap head $ callFunc loccall

          eval (SynArrow expr ident) = obtStructVal expr ident

          eval (SynMat mat) = obtMatVal mat

          eval (SynIndex e idxs) = obtIndexVal e idxs

          eval (SynPar e) = evalExpr e
          eval (SynExp e1 e2) = evalBin expVal e1 e2
          eval (SynNeg e) = evalUn negVal e
          eval (SynBitNot e) = evalUn bitNotVal e
          eval (SynTimes e1 e2) = evalBin timesVal e1 e2
          eval (SynDiv e1 e2) = evalBin divVal e1 e2
          eval (SynDot e1 e2) = evalBin dotVal e1 e2
          eval (SynDotTimes e1 e2) = evalBin dotTimesVal e1 e2
          eval (SynDotDiv e1 e2) = evalBin dotDivVal e1 e2
          eval (SynMod e1 e2) = evalBin modVal e1 e2
          eval (SynPlus e1 e2) = evalBin plusVal e1 e2
          eval (SynMinus e1 e2) = evalBin minusVal e1 e2
          eval (SynRShift e1 e2) = evalBin rshiftVal e1 e2
          eval (SynLShift e1 e2) = evalBin lshiftVal e1 e2
          eval (SynEQ e1 e2) = evalBin eqVal e1 e2
          eval (SynNEQ e1 e2) = evalBin neqVal e1 e2
          eval (SynLT e1 e2) = evalBin ltVal e1 e2
          eval (SynLE e1 e2) = evalBin leVal e1 e2
          eval (SynGT e1 e2) = evalBin gtVal e1 e2
          eval (SynGE e1 e2) = evalBin geVal e1 e2
          eval (SynBitAnd e1 e2) = evalBin bitAndVal e1 e2
          eval (SynBitXor e1 e2) = evalBin bitXorVal e1 e2
          eval (SynBitOr e1 e2) = evalBin bitOrVal e1 e2
          eval (SynNot e) = evalUn notVal e
          eval (SynAnd e1 e2) = evalBin andVal e1 e2
          eval (SynXor e1 e2) = evalBin xorVal e1 e2
          eval (SynOr e1 e2) = evalBin orVal e1 e2



-- = Statements


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
    in do raiseScope
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
-- Includes the list = function() case
runAttr :: (Located SynAttr) -> Exec ()
runAttr locattr =
    do let attr = ignorepos locattr
           locidents = getAttrVars attr 
           locexprs = getAttrExprs attr
       if (length locidents > 0) && (length locexprs == 1)
       then
        let expr0 = ignorepos $ head locexprs
         in case expr0 of
              (SynCallExpr loccall) ->
                  do vals <- callFunc loccall
                     let names = map (getlabel . ignorepos) locidents
                     sequence_ $ zipWith modifyVarValue names vals
              _ -> distRunAttr locattr
       else distRunAttr locattr


-- | Basic attribution execution, with a list of identifiers
-- and a list of expressions of the same length
distRunAttr :: (Located SynAttr) -> Exec ()
distRunAttr locattr =
    do let attr = ignorepos locattr
           locidents = getAttrVars attr 
           locexprs = getAttrExprs attr
       vals <- mapM evalExpr locexprs
       let names = map (getlabel . ignorepos) locidents
       zipWithM_ modifyVarValue names vals


-- | Statement execution
runStmt :: (Located SynStmt) -> Exec ()
runStmt locstmt =
    do let stmt = ignorepos locstmt
       case stmt of
         (SynStmtDef locdef) -> runDef locdef
         (SynStmtAttr locattr) -> runAttr locattr
         (SynStmtIf locif) -> runIf locif
         (SynStmtWhile locwhile) -> runWhile locwhile
         (SynStmtCall loccall) -> callProc loccall
         _ -> return ()


-- | Block execution
runBlock :: (Located SynBlock) -> Exec ()
runBlock locblock =
    do let block = ignorepos locblock
       mapM_ runStmt $ getStmts block

-- = Subprograms

-- | Add procedure/function arguments to variable table
registerArgs :: [SynTypedIdent] -> [(Val, Maybe MemLoc)] -> Exec ()
registerArgs formalArgs vals =
    zipWithM_ regVar formalArgs vals
    where
        regVar :: SynTypedIdent -> (Val, Maybe MemLoc) -> Exec ()
        regVar tid (val, mloc) =
            do let vname = getName tid
                   ref = isRef $ toAnnType tid
               if ref
               then case mloc of
                 Just loc -> registerLocalRef vname (toType val) loc
                 Nothing -> error "invalid reference expression" 
               else registerLocalVar vname (toType val) val


-- | Add function returns to variable table
registerRets :: [SynTypedIdent] -> Exec ()
registerRets rets = 
    let regVar :: SynTypedIdent -> Exec ()
        regVar tid =
            do let vname = getName tid
               vtype <- findType . ignorepos . getTypedIdentType $ tid
               registerLocalUndefVar vname vtype
     in mapM_ regVar rets


-- | Get address for expression, if lvalue
-- TODO: Add struct acess
extrAddr :: LocSynExpr -> Exec (Maybe MemLoc)
extrAddr locexpr =
    case expr of
      (SynIdentExpr locident) ->
          fmap (Just . getVarAddr) . findVar . getName $ locident
      _ -> return Nothing
    where expr = ignorepos locexpr


-- == Structures

-- | Create constructor function for structure
mkConstr :: Type -> Func
mkConstr st@(StructType name ents) = NativeFunc name ftype constr
    where argtypes = map snd ents
          ftype = FuncType [st] (fmap toAnnType argtypes)
          constr vals = return $ [StructVal st vals]


-- == Procedures

-- | Execute procedure
-- No preparation is made by this function
runProc :: Proc -> Exec ()
runProc (NativeProc _ _ x) = x []
runProc (Proc p) = runBlock $ getProcBlock p


-- | Procedure call
callProc :: Located SynCall -> Exec ()
callProc loccall =
    let call = ignorepos loccall
        args = getexprlist . getArgList $ call
     in do argAddrs <- mapM extrAddr args
           argValues <- mapM evalExpr args
           vt <- saveAndClearScope
           let pname = getName . getFuncId $ call
               argtypes = map toAnnType $ toTypeList argValues
               ptype = ProcType argtypes
               argzip = zip argValues argAddrs
           proc <- findProc pname ptype
           case proc of
             (NativeProc _ _ x) -> x argValues
             (Proc sp) -> do registerArgs (getProcArgs sp) argzip
                             runBlock $ getProcBlock sp
           modifyVarTable vt


-- = Functions


-- | Function call
callFunc :: Located SynCall -> Exec [Val]
callFunc loccall =
    let call = ignorepos loccall
        args = getexprlist . getArgList $ call
     in do argAddrs <- mapM extrAddr args
           argValues <- mapM evalExpr args
           vt <- saveAndClearScope
           let argTypes = toTypeList argValues
               fname = getName . getFuncId $ call
               argzip = zip argValues argAddrs
           func <- findFunc fname argTypes
           rets <- case func of
             (NativeFunc _ _ x) ->
                 x argValues 
             (Func sf) -> do
                 registerArgs (getFuncArgs sf) argzip
                 registerRets (getFuncRet sf) 
                 runBlock $ getFuncBlock sf
                 let retNames = map getName $ getFuncRet sf
                 mapM obtainVarValue retNames
           modifyVarTable vt
           return rets


-- = Module Execution

-- | Load builtin procedures and functions
loadNativeSymbols :: Exec ()
loadNativeSymbols =
    do mapM_ registerProc nativeProcs
       mapM_ registerFunc nativeFuncs
       mapM_ registerStruct nativeStructs
       mapM_ (registerFunc . mkConstr) nativeStructs


-- | Load global variables, procedures, functions ans structs
loadModuleSymbols :: SynModule -> Exec ()
loadModuleSymbols mod = mapM_ loadSymbol (modStmts mod)
    where
        loadSymbol (SynModDef locdef) = return ()

        loadSymbol (SynModProc locproc) =
            registerProc $ Proc $ ignorepos locproc

        loadSymbol (SynModFunc locfunc) = 
            registerFunc $ Func $ ignorepos locfunc

        loadSymbol (SynModStruct locstruct) =
            do let sttype = toType locstruct
               registerStruct sttype
               registerFunc $ mkConstr sttype
        

-- | Module execution
runmodule :: SynModule -> Exec ()
runmodule m =
    do loadNativeSymbols
       loadModuleSymbols m
       main <- findProc "main" (ProcType [])
       runProc main 
