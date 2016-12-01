module StaticAnalyzer where

import qualified Lexical as L
import Syntactic
import PosParsec(ignorepos, Located)
import Types

--------------------------------------
--------- Build symbol table ---------
--------------------------------------
-- This function traverses the syntactic tree
-- building the symbol table. It searches for
-- definitions of all kinds - SynDef, SynFunc, 
-- SynProc, SynStruct - then builds a list of
-- identifiers altogether with its type (and 
-- any other information that might be useful).
data STEntry = Variable { getVarId :: String    -- identifier
                        , getVarType :: String  -- type
                        , getVarLevel :: Int    -- scope level
                        , isMutable :: Bool }   -- is it marked as mut/const?
             | Function { getFuncName :: String
                        , getFuncArgTypes :: [String]
                        , getFuncRetTypes :: [String] } 
             | Procedure { getProcName :: String
                         , getProcArgTypes :: [String] } deriving (Show)

type SymbolTable = [STEntry]

symbolTable :: SymbolTable
symbolTable = [Procedure "print" ["int"]
              ,Procedure "print" ["float"]
              ,Procedure "print" ["mat"]
              ,Procedure "print" ["bool"]
              ,Procedure "print" ["string"]
              ,Procedure "println" ["int"]
              ,Procedure "println" ["float"]
              ,Procedure "println" ["mat"]
              ,Procedure "println" ["bool"]
              ,Procedure "println" ["string"]

              ,Function "floor" ["float"] ["int"]
              ,Function "ceil" ["float"] ["int"]
              ,Function "round" ["float"] ["int"]
              ,Function "toFloat" ["int"] ["float"]
              ,Function "toBool" ["int"] ["bool"]
              ,Function "toString" ["int"] ["string"]
              ,Function "toString" ["float"] ["string"]
              ,Function "readInt" [] ["int"]
              ,Function "readFloat" [] ["float"]

              ,Function "vec2" (floatList 2) ["vec2"]
              ,Function "vec3" (floatList 3) ["vec3"]
              ,Function "vec4" (floatList 4) ["vec4"]
              ,Function "mat2" (floatList 4) ["mat2"]
              ,Function "mat3" (floatList 9) ["mat3"]
              ,Function "mat4" (floatList 16) ["mat4"] ]
                where floatList n = ["float" | _ <- [1..n]]

data Field = Field { getFieldName :: String
                   , getFieldType :: String
                   , fieldMutable :: Bool } deriving (Show)

data UTEntry = StructEntry { getStructId :: String     -- Type name
                          , getStructFields :: [Field] } -- List of fields defined inside this struct
             | Primitive { getPrimName :: String } deriving (Show)

typeTable :: UserTypeTable
typeTable = [ Primitive "int"
            , Primitive "float"
            , Primitive "bool"
            , Primitive "vec"
            , Primitive "vec2"
            , Primitive "vec3"
            , Primitive "vec4"
            , Primitive "mat"
            , Primitive "mat2"
            , Primitive "mat3"
            , Primitive "mat4"
            , Primitive "string"]

type UserTypeTable = [UTEntry]

type SuperTable = (SymbolTable, UserTypeTable)

-- TODO: We should receive a SynProgram, which is
-- itself a set of SynModules
stFromModule :: SuperTable -> SynModule -> Either String SuperTable 
stFromModule st ( SynModule id stmts ) = stFromModStmts st stmts 

stFromModStmts :: SuperTable -> [SynModStmt] -> Either String SuperTable
stFromModStmts st [] = Right st
stFromModStmts st (h:t) = let result = stFromModStmt st h in
                             case result of
                                Left errorMsg  -> Left errorMsg
                                Right newSt -> stFromModStmts newSt t

-- Statements inside a module are global and thus have
-- scope level zero.
stFromModStmt :: SuperTable -> SynModStmt -> Either String SuperTable
stFromModStmt st s = case s of
                            (SynModStruct stct) -> stFromStruct st (ignorepos stct)
                            (SynModDef def) -> stFromDef st 0 (ignorepos def)  
                            (SynModProc proc) -> stFromProc st (ignorepos proc)
                            (SynModFunc func) -> stFromFunc st (ignorepos func) 

-- Remember we don't have nested functions/procedures, so they'll always
-- lie at scope level zero. Also, structs can't be defined inside subroutines,
-- so it also must lie at scope level zero.
stFromStruct :: SuperTable -> SynStruct -> Either String SuperTable 
stFromStruct st stct = if isElemUserTypeTable structName (snd st) || isElemSymbolTable structName 0 (fst st)
                        then Left $ "Name already being used as a type name or variable name:" ++ structName
                        else Right (newSymbolTable, newTypeTable)
                       where
                        structName = getlabel $ ignorepos $ getStructName stct
                        newSymbolTable = (makeStructConstructor stct) : (fst st)
                        newTypeTable = newTTEntry : (snd st)
                        newTTEntry = StructEntry (getlabel $ ignorepos $ getStructName stct) structFields
                        structFields = buildFieldsList (getTuple stct)

buildFieldsList :: [SynTypedIdent] -> [Field]
buildFieldsList [] = []
buildFieldsList (h:t) = entry : (buildFieldsList t)
                        where
                          nam = getlabel . ignorepos . getTypedIdentName $ h
                          typ = getlabel . ignorepos . getTypeIdent . ignorepos . getTypedIdentType $ h
                          annot = interpretAnnotation (getAnnotation . ignorepos . getTypedIdentType $ h) True
                          entry = Field nam typ annot

makeStructConstructor :: SynStruct -> STEntry
makeStructConstructor stct = Function name args (name:[])
                              where
                                name = getlabel . ignorepos . getStructName $ stct
                                args = (getlabel . ignorepos 
                                                 . getTypeIdent 
                                                 . ignorepos 
                                                 . getTypedIdentType) `fmap` getTuple stct

stFromDef :: SuperTable -> Int -> SynDef -> Either String SuperTable
stFromDef st lvl (SynDef typedId) = stFromTypedIdentList st True lvl typedId

stFromProc :: SuperTable -> SynProc -> Either String SuperTable
stFromProc st  (SynProc name _ formalParam block) = let n = getlabel $ ignorepos name in
                                                          if isElemUserTypeTable n  (snd st) || isElemSymbolTable n 0 (fst st)
                                                            then Left "Name already being used as a type name or variable name."
                                                            else Right (syt, (snd st))
                                                                where syt = entry : (fst st)
                                                                      entry = Procedure (getlabel $ ignorepos name)
                                                                                        (buildProcTypeStr formalParam)

stFromFunc :: SuperTable -> SynFunc -> Either String SuperTable
stFromFunc st  (SynFunc name _ formalParam ret block) = let n = getlabel $ ignorepos name in
                                                        if isElemUserTypeTable n (snd st) || isElemSymbolTable n 0 (fst st)
                                                        then Left "Name already being used as a type name or variable name."
                                                        else Right (syt, (snd st))
                                                                where syt = entry : (fst st)
                                                                      entry = Function (getlabel $ ignorepos name)
                                                                                       (buildFuncArgTypeList formalParam)
                                                                                       (buildFuncRetTypeList ret)
                                                                                               

buildFuncArgTypeList :: [SynTypedIdent] -> [String]
buildFuncArgTypeList formalParam = (getlabel . ignorepos . getTypeIdent . ignorepos . getTypedIdentType) `fmap` formalParam
                                        
buildFuncRetTypeList :: [SynTypedIdent] -> [String]
buildFuncRetTypeList ret = (getlabel . ignorepos . getTypeIdent . ignorepos . getTypedIdentType) `fmap` ret

buildProcTypeStr :: [SynTypedIdent] -> [String]
buildProcTypeStr formalParam = (getlabel . ignorepos . getTypeIdent . ignorepos . getTypedIdentType) `fmap` formalParam

-- Bool parameter (defaultMut) tells stFromTypedIdentList whether we're parsing
-- a list whose "default mode" is to consider everything mutable or constant.
stFromTypedIdentList :: SuperTable -> Bool -> Int -> [SynTypedIdent] -> Either String SuperTable
stFromTypedIdentList st defaultMut lvl [] = Right st
stFromTypedIdentList st defaultMut lvl (h:t) = let name = getlabel $ ignorepos $ getTypedIdentName h in
                                        if isElemUserTypeTable name (snd st) || isElemSymbolTable name lvl (fst st)
                                            then Left "Name already being used as a type name or variable name." 
                                            else stFromTypedIdentList (newST, snd st) defaultMut lvl t
                                                  where newST = entry : (fst st)
                                                        entry = Variable (getlabel $ ignorepos $ getTypedIdentName h) 
                                                                         (getLabelFromType $ ignorepos $ getTypedIdentType h)
                                                                          lvl
                                                                          (interpretAnnotation annot defaultMut)
                                                        annot = getAnnotation $ ignorepos $ getTypedIdentType h

-- Reads annotation (or absense of it) and decides whether variable
-- is mutable or constant.
interpretAnnotation :: Maybe (Located SynAnnotation) -> Bool -> Bool
interpretAnnotation Nothing defaultMut = defaultMut
interpretAnnotation (Just lSA) defaultMut = case getModifier $ ignorepos lSA of
                                              Nothing -> defaultMut
                                              Just token -> case getlex $ ignorepos token of
                                                              L.LexModMut -> True
                                                              L.LexModConst -> False

isElemUserTypeTable :: String -> [UTEntry] -> Bool
isElemUserTypeTable s [] = False
isElemUserTypeTable s (h:t) = case h of 
                                StructEntry name _ -> if name == s 
                                                        then True
                                                        else isElemUserTypeTable s t 
                                Primitive name -> if name == s
                                                    then True
                                                    else isElemUserTypeTable s t

isElemSymbolTable :: String  -> Int -> [STEntry] -> Bool
isElemSymbolTable s lvl [] = False
isElemSymbolTable s lvl (h:t) = case h of 
                                  Variable name _ varlvl _ -> if name == s && lvl == varlvl
                                                                then True
                                                                else isElemSymbolTable s lvl t 
                                  Function name _ _ -> if name == s
                                                        then True
                                                        else isElemSymbolTable s lvl t
                                  Procedure name _ -> if name == s
                                                        then True
                                                        else isElemSymbolTable s lvl t

-----------------------------------
--------- Static analyzer --------- TODO: Move this to another file when 
-----------------------------------       things get more structured.
-- Checking rules: each of these rules check SynStuff for
-- soundness (according to soundness rules of each syntactic construct).
-- The general idea is: the soundness of a syntactic construct
-- depends on the soundness of what constitutes it; hence, when we have
-- a syntactic unit S -> s1 s2 s3 ..., we check each syntactic subunit
-- s1, s2, s3. If all of them are sound (i.e., all of them return RIGHT),
-- we can say that S itself is sound. If any rule is wrong, 'though (i.e.,
-- returns a LEFT), we say that S is also wrong and we forward the message
-- inside LEFT.
--
-- IDEA: What if each Syn___ was instance of some Checkable typeclass,
-- which is itself a monad, so we could chain Checkable stuff together?
-- Would this eliminate all of those case blabla?
--
-- IDEA: Maybe we could chain the error messages to say something like
-- "In module x, in func ___, in statement ___ : blablabla", just as
-- Haskell compiler does.
--
checkMod :: SynModule -> Either String SynModule
checkMod sm@(SynModule id stmts) = case stFromModule (symbolTable, typeTable) sm of
                                    Left msg -> Left msg
                                    Right st -> case checkModStmts st stmts of
                                                  Right _ -> Right (SynModule id stmts)
                                                  Left msg -> Left msg

checkModStmts :: SuperTable -> [SynModStmt] -> Either String [SynModStmt]
checkModStmts st [] = Right []
checkModStmts st (stmt:tail) = case checkedStmt of
                            Left msg -> Left msg
                            Right _ -> do x <- checkedStmt
                                          y <- checkModStmts st tail
                                          Right (x : y)
                          where checkedStmt = checkModStmt st stmt

checkModStmt :: SuperTable -> SynModStmt -> Either String SynModStmt
checkModStmt st stmt = case stmt of
                  SynModStruct ss -> Right stmt    -- No rule for structs nor def's (so far)
                  SynModDef sd -> Right stmt
                  SynModProc sp -> case checkProc st sp of
                                      Right _ -> Right stmt
                                      Left msg -> Left msg
                  SynModFunc sf -> case checkFunc st sf of
                                      Right _ -> Right stmt
                                      Left msg -> Left msg

-- Again: no nested subroutines, so scope level is always 1.
checkFunc :: SuperTable -> Located SynFunc -> Either String SynFunc
checkFunc st func = do stWithArgs <- stFromTypedIdentList st False lvl funcArgs
                       stWithRet <- stFromTypedIdentList stWithArgs True lvl funcRet
                       checkBlock stWithRet lvl funcBlock
                       return $ ignorepos func
                    where
                      lvl = 1
                      funcArgs = getFuncArgs $ ignorepos func
                      funcRet = getFuncRet $ ignorepos func
                      funcBlock = getFuncBlock $ ignorepos func

checkProc :: SuperTable -> Located SynProc -> Either String SynProc
checkProc st proc = case checkBlock newSt lvl $ getProcBlock unlocProc of
                      Right _ -> Right unlocProc
                      Left msg -> Left msg
                    where lvl = 1
                          unlocProc = ignorepos proc
                          newSt = case stFromTypedIdentList st False lvl (getProcArgs $ ignorepos proc) of
                                    Right newSt' -> newSt'
                                    Left msg -> st -- PENDING: We should do something to throw message in case
                                                   -- of error here!

checkBlock :: SuperTable -> Int -> Located SynBlock -> Either String SynBlock
checkBlock st lvl block = case checkedStmts of
                          Right _ -> Right $ ignorepos block
                          Left msg -> Left msg
                        where checkedStmts = checkStmts st lvl (ignorepos `fmap` getStmts (ignorepos block))

checkStmts :: SuperTable -> Int -> [SynStmt] -> Either String [SynStmt]
checkStmts st lvl [] = Right []
checkStmts st lvl (stmt:tail) = case fst checkedStmt of
                                  Left msg -> Left msg
                                  Right _ -> do h <- fst checkedStmt
                                                t <- checkStmts (snd checkedStmt) lvl tail
                                                Right (h:t)
                                where checkedStmt = checkStmt st lvl stmt

-- TODO: [LUÍS] Isso é uma gambiarra horrorosa que teve de ser feita porque
-- não lembrei que a tabela vai mudar dentro dos blocos. Com sorte o problema
-- não vai se propagar. A ideia é que retornamos não só o Either mas também
-- a tabela de símbolos atualizada, caso um Def tenha sido encontrado dentro
-- da função. Essa tabela atualizada é a que é propagada pros statement 
-- subsequentes. Tentei fazer o conserto mais rápido e menos danoso ao resto
-- do código possível. Lembrar de consertar isso depois.
--
-- UPDATE: Quick reminder for the future: while checking a IF/WHILE's block, 
-- we shall increment the scope level. 
checkStmt :: SuperTable -> Int -> SynStmt -> (Either String SynStmt, SuperTable)
checkStmt st lvl s = case s of
                      SynStmtAttr sa -> case checkAttr st $ ignorepos sa of
                                          Left msg -> (Left msg, st)
                                          Right _ -> (Right s, st) 
                      SynStmtDefAttr sda -> (Right s, st)
                      SynStmtIf si -> case checkIf st lvl (ignorepos si) of
                                        Left msg -> (Left msg, st)
                                        Right _ -> (Right s, st) 
                      SynStmtWhile sw -> case checkWhile st lvl (ignorepos sw) of
                                          Left msg -> (Left msg, st)
                                          Right _ -> (Right s, st)                     
                      SynStmtCall sc -> case checkCall st lvl (ignorepos sc) of 
                                          Left msg -> (Left msg, st)
                                          Right _ -> (Right s, st)
                      SynStmtDef sd -> case eitherNewSt of
                                          Right newSt -> (Right s, newSt)
                                          Left msg -> (Left msg, st)
                                        where 
                                          eitherNewSt = stFromDef st lvl def -- PENDING! Statement must carry scope level
                                          def = ignorepos sd

checkCall :: SuperTable -> Int -> SynCall -> Either String SynCall
checkCall st lvl sc = do stEntry <- searchSTEntry (fst st) procName
                         procArgs <- buildExprTypeList st (ignorepos `fmap` (getexprlist $ getArgList sc))
                         case stEntry of
                            Procedure _ _ -> if (procInST (fst st) procName procArgs) == True
                                              then return sc
                                              else fail $ "No such procedure: " ++ (show procName) ++ "(" ++ (show procArgs) ++ ")."
                            _ -> fail $ "Not a procedure: " ++ (show sc)
                      where 
                        procName = (getlabel $ ignorepos $ getFuncId sc)

checkIf :: SuperTable -> Int -> SynIf -> Either String SynIf
checkIf st lvl (SynIf [] elseBlock) = case elseBlock of
                                        Nothing -> Right out
                                        Just block -> do checkBlock st lvl block
                                                         return out
                                      where out = SynIf [] elseBlock

checkIf st lvl (SynIf (expBlk:tail) elseBlock) = do checkExpBlk st lvl expBlk
                                                    checkIf st lvl (SynIf tail elseBlock)

checkExpBlk :: SuperTable -> Int -> (Located SynExpr, Located SynBlock) -> Either String Bool
checkExpBlk st lvl (exp, blk) = do t <- checkExpr st (ignorepos exp)
                                   checkBlock st (lvl+1) blk
                                   if t == ["bool"] 
                                    then return True
                                    else fail $ "Not a bool expression in IF clause: " ++ (show exp)

-- [LUIS] FINALLY! Check https://www.schoolofhaskell.com/school/
-- starting-with-haskell/basics-of-haskell/10_Error_Handling
-- for the correct way of dealing with Either.
checkWhile :: SuperTable -> Int -> SynWhile -> Either String SynWhile
checkWhile st lvl sw = case exprOk of
                          Left msg -> Left msg
                          Right x -> if x == ["bool"]
                                      then case blockOk of
                                              Left msg -> Left msg
                                              Right _ -> Right sw
                                      else Left $ "Not a bool expression in WHILE clause: " ++ (show $ getWhileCondition sw)
                        where
                          exprOk = checkExpr st (ignorepos $ getWhileCondition sw)
                          blockOk = checkBlock st (lvl+1) (getWhileBlock sw)

checkAttr :: SuperTable -> SynAttr -> Either String SynAttr
checkAttr st sa@(SynAttr si se) = do assertListMutable st lvalues
                                     s1 <- buildIdentTypeList st lvalues
                                     s2 <- buildExprTypeList st (ignorepos `fmap` se)
                                     if typeListsEqual s1 s2 == True
                                        then Right sa
                                        else Left "Variable and expression have different types in attribution."
                                    where
                                      lvalues = ignorepos `fmap` si

assertListMutable :: SuperTable -> [SynLValue] -> Either String ()
assertListMutable st [] = return ()
assertListMutable st (h:t) = do assertMutable st h
                                assertListMutable st t
                                return ()

assertMutable :: SuperTable -> SynLValue -> Either String ()
assertMutable st (SynLIdent f) = do var <- searchSTEntry (fst st) lb
                                    if isMutable var
                                      then return ()
                                      else fail $ "Variable is not mutable: " ++ lb
                                 where
                                    lb = getlabel . ignorepos $ f

-- Both field inside the struct and the struct itself
-- must be mutable. For example, if we have a->b where
-- 'b' is marked as mutable but 'a' is const, a->b will
-- be const and therefore we won't let it appear in the
-- left side of an attribution. 
assertMutable st (SynLArrow p f) = do pathMut <- assertMutable st (ignorepos p)
                                      pathTyp <- fieldTypeInLValue st (ignorepos p) 
                                      entry <- searchStructEntry (snd st) pathTyp
                                      field <- searchFieldInStruct (getStructFields entry) fieldName
                                      
                                      if fieldMutable field
                                        then return ()
                                        else fail $ "Field or variable is not mutable: " ++ (show p) ++ " , " ++ (show f)
                                   where
                                     fieldName = getlabel . ignorepos $ f

assertMutable st (SynLIndex p i) = assertMutable st (ignorepos p)

-----------------------
searchStructEntry :: UserTypeTable -> String -> Either String UTEntry
searchStructEntry [] name = fail $ "Struct not defined: " ++ name
searchStructEntry (h:t) name = case h of
                                  se@(StructEntry n _) -> if n == name
                                                          then return se
                                                          else searchTheRest
                                  Primitive _ -> searchTheRest
                               where
                                  searchTheRest = searchStructEntry t name

searchFieldInStruct :: [Field] -> String -> Either String Field
searchFieldInStruct [] name = fail $ "Field not defined: " ++ name
searchFieldInStruct (h:t) name = if getFieldName h == name
                                  then return h
                                  else searchFieldInStruct t name
                                                                
typeListsEqual :: [String] -> [String] -> Bool
typeListsEqual [] [] = True
typeListsEqual (h1:t1) (h2:t2) = if h1 /= h2 && h1 /= "_"
                                    then False
                                    else typeListsEqual t1 t2

buildIdentTypeList :: SuperTable -> [SynLValue] -> Either String [String]
buildIdentTypeList st [] = return []
buildIdentTypeList st (h:t) = do typeH <- fieldTypeInLValue st h
                                 typeT <- buildIdentTypeList st t
                                 return (typeH : typeT) 

fieldTypeInLValue :: SuperTable -> SynLValue -> Either String String
fieldTypeInLValue st (SynLIdent i) = identType st (ignorepos i)
fieldTypeInLValue st (SynLArrow p f) = do path <- fieldTypeInLValue st (ignorepos p)
                                          entry <- searchStructEntry (snd st) path
                                          field <- searchFieldInStruct (getStructFields entry) fieldName
                                          return $ getFieldType field
                                       where
                                          fieldName = getlabel . ignorepos $ f
fieldTypeInLValue st (SynLIndex p i) = do checkExprList st (ignorepos `fmap` i)
                                          return "float" -- TEMPORARY, because matrices are always float

checkExprList :: SuperTable -> [SynExpr] -> Either String ()
checkExprList st [] = return ()
checkExprList st (h:t) = do checkExpr st h
                            checkExprList st t

buildExprTypeList :: SuperTable -> [SynExpr] -> Either String [String]
buildExprTypeList st [] = Right []
buildExprTypeList st (h:t) = case checkExpr st h of
                                Left msg -> Left msg
                                Right s -> case buildExprTypeList st t of
                                            Left msg -> Left msg
                                            Right l -> Right $ s ++ l

identType :: SuperTable -> SynIdent -> Either String String
identType st si@(SynIdent s) = if s == "_"
                                          then Right s
                                          else case searchSymbolTable (fst st) s of
                                            Right vtype -> Right vtype
                                            Left msg -> Left msg

-- PENDING !!! [LUÍS] Verificar o nível do símbolo ao buscar
searchSymbolTable :: [STEntry] -> String -> Either String String
searchSymbolTable [] s = Left $ "Variable not defined: " ++ s
searchSymbolTable (h:t) s = case h of 
                                Variable name vtype _ _ -> if name == s
                                                            then if vtype /= "int" && vtype /= "float" && vtype /= "bool" && vtype /= "vec" && 
                                                                    vtype /= "vec2" && vtype /= "vec3" && vtype /= "vec4" && vtype /= "mat" && 
                                                                    vtype /= "mat2" && vtype /= "mat3" && vtype /= "mat4" && vtype /= "string"
                                                                    then Right $ "struct " ++ vtype
                                                                    else Right vtype
                                                            else searchSymbolTable t s
                                _ -> searchSymbolTable t s

searchSTEntry :: [STEntry] -> String -> Either String STEntry
searchSTEntry [] s = Left $ "Symbol \"" ++ s ++ "\" was not defined."
searchSTEntry (h:t) s = case h of
                          Variable name _ _ _ -> if name == s
                                                  then Right h
                                                  else searchSTEntry t s
                          Function name _ _ -> if name == s
                                                then Right h
                                                else searchSTEntry t s
                          Procedure name _ -> if name == s
                                                then Right h
                                                else searchSTEntry t s

-- This new function takes into account the type (this was needed
-- to implement procedure overloading)
procInST :: [STEntry] -> String -> [String] -> Bool
procInST [] pName pArgs = False
procInST (h:t) pName pArgs = case h of
                              Variable _ _ _ _ -> searchTheRest
                              Function _ _ _ -> searchTheRest
                              Procedure name args -> if name == pName && args == pArgs
                                                        then True
                                                        else searchTheRest
                             where searchTheRest = procInST t pName pArgs 

{-searchUserTypeTable :: [UTEntry] -> String -> Either String String
searchUserTypeTable [] s = Left $ "Variable not defined: " ++ s
searchUserTypeTable (h:t) s = case h of
                                StructEntry name _ -> if name == s
                                                          then Right $ "struct " ++ 
                                                          else searchUserTypeTable t s
                                _ -> searchUserTypeTable t s-}

searchStructField :: [UTEntry] -> String -> String -> Either String [String]
searchStructField [] struct field = Left $ "Field not defined: [" ++ field ++ "] in struct [" ++ struct ++ "]"
searchStructField (h:t) struct field = case h of
                                        StructEntry name l -> if name == struct
                                                                  then structFieldType l field
                                                                  else searchStructField t struct field
                                        _ -> searchStructField t struct field

structFieldType :: [Field] -> String -> Either String [String]
structFieldType [] f = Left "Variable not defined."
structFieldType (h:t) f = if f == getFieldName h
                            then Right $ getFieldType h : []
                            else structFieldType t f


checkExpr :: SuperTable -> SynExpr -> Either String [String]
checkExpr st se = case se of
                  SynIdentExpr e -> case identType st $ ignorepos e of
                                      Left msg -> Left msg
                                      Right vtype -> Right $ vtype : []
                  SynLitIntExpr _ -> Right ["int"]
                  SynLitFloatExpr _ -> Right ["float"]
                  SynLitBoolExpr _ -> Right ["bool"]
                  SynLitStrExpr _ -> Right ["string"]
                  
                  SynCallExpr e -> do checkFuncParam st (ignorepos e)
                                      findFuncRetTypes (fst st) $ getlabel $ ignorepos $ getFuncId $ ignorepos e
  
                  SynPar e -> checkExpr st $ ignorepos e
                  SynNeg e -> case checkExpr st $ ignorepos e of
                                Left msg -> Left msg
                                Right l -> if l == ["int"] || l == ["float"]
                                              then Right l
                                              else Left "Operator '-' (unary) expects an operand of type int or float."
                  SynBitNot e -> case checkExpr st $ ignorepos e of
                                Left msg -> Left msg
                                Right l -> if l == ["int"]
                                            then Right l
                                            else Left "Operator '!' expects an operand of type int."
                  SynNot e -> case checkExpr st $ ignorepos e of
                                Left msg -> Left msg
                                Right l -> if l == ["bool"]
                                            then Right l
                                            else Left "Operator 'not' expects an operand of type bool."
                  SynArrow e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if (head $ words $ head l1) == "struct"
                                                      then searchStructField (snd st) (last $ words $ head l1) $ getlabel $ ignorepos e2
                                                      else Left "Operator '->' expects a struct as a left operand."
                  SynExp e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "Operator '^' expects operands of equal types."
                                                                        else Left "Operator '^' expects operands of type int or float."
                                                  else Left "Operator '^' expects operands of type int or float."
                  SynTimes e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "If the secon operand is of type int or float, operator '*' expects a first operand of same type."
                                                                        else if (head $ words $ head l2) == "mat"
                                                                                then if l1 == ["float"]
                                                                                      then Right l2
                                                                                      else Left "If the second operand is of type mat, operator '*' expects a first operand of type float or mat."
                                                                                else Left "If the first operand is of type int or float, operator '*' expects a second operand of type int, float or mat."
                                                    else if (head $ words $ head l1) == "mat"
                                                          then case checkExpr st $ ignorepos e2 of
                                                                Left msg -> Left msg
                                                                Right l2 -> if l2 == ["float"]
                                                                              then Right l1
                                                                              else if (head $ words $ head l2) == "mat"
                                                                                      then if (last $ words $ head l1) == (head $ tail $ words $ head l2)
                                                                                            then Right $ ("mat " ++ (head $ tail $ words $ head l1) ++ " " ++ (last $ words $ head l2)) : []
                                                                                            else Left "When multiplying matrices, number of columns of the first one must be equal to the number of lines of the second one."
                                                                                      else Left "If the first operando is of type mat, operator '*' expects a second operand of type float or mat."
                                                          else Left "Operator '*' expects operands of type int, float or mat."
                  SynDiv e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "If the first operand is of type int or float, operator '/' expects second operator of same type."
                                                                        else Left "If the first operand is of type int or float, operator '/' expects second operator of type int or float."
                                                  else if (head $ words $ head l1) == "mat"
                                                          then case checkExpr st $ ignorepos e2 of
                                                                  Left msg -> Left msg
                                                                  Right l2 -> if l2 == ["float"]
                                                                                then Right l1 
                                                                                else Left "If the first operand is of type mat, operator '/' expects second operator of type float."
                                                          else Left "Operator '/' expects operands of type int, float or mat."
                  SynMod e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "Operator 'mod' expects operands of equal types."
                                                                        else Left "Operator 'mod' expects operands of type int or float."
                                                  else Left "Operator 'mod' expects operands of type int or float."
                  SynPlus e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 /= ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 /= ["bool"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "Operator '+' expects operands of equal types."
                                                                        else Left "Operator '+' expects operands of type int, float or mat."
                                                  else Left "Operator '+' expects operands of type int, float or mat."
                  SynMinus e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 /= ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 /= ["bool"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "Operator '-' (binary) expects operands of equal types."
                                                                        else Left "Operator '-' (binary) expects operands of type int, float or mat."
                                                  else Left "Operator '-' (binary) expects operands of type int, float or mat."
                  SynDotTimes e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if (head $ words $ head l1) == "mat"
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if (head $ words $ head l2) == "mat"
                                                                            then if l1 == l2
                                                                                    then Right l1
                                                                                    else Left "Operator '.*.' expects operands of equal types."
                                                                            else Left "Operator '.*.' expects operands of type mat."
                                                      else Left "Operator '.*.' expects operands of type mat."
                  SynDotDiv e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if (head $ words $ head l1) == "mat"
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if (head $ words $ head l2) == "mat"
                                                                            then if l1 == l2
                                                                                    then Right l1
                                                                                    else Left "Operator './.' expects operands of equal types."
                                                                            else Left "Operator './.' expects operands of type mat."
                                                      else Left "Operator './.' expects operands of type mat."
                  SynDot e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if (head $ words $ head l1) == "mat"
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if (head $ words $ head l2) == "mat"
                                                                            then if l1 == l2
                                                                                    then if (head $ tail $ words $ head l1) == "1" || (last $ words $ head l1) == "1"
                                                                                          then Right l1
                                                                                          else Left "Operator '.' expects matrices with one line or one column."
                                                                                    else Left "Operator '.' expects operands of equal types."
                                                                            else Left "Operator '.' expects operands of type mat."
                                                      else Left "Operator '.' expects operands of type mat."
                  SynRShift e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then if l1 == l2
                                                                                  then Right l1
                                                                                  else Left "Operator '>>' expects operands of equal types."
                                                                          else Left "Operator '>>' expects operands of type int."
                                                    else Left "Operator '>>' expects operands of type int."
                  SynLShift e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then if l1 == l2
                                                                                  then Right l1
                                                                                  else Left "Operator '<<' expects operands of equal types."
                                                                          else Left "Operator '<<' expects operands of type int."
                                                    else Left "Operator '<<' expects operands of type int."
                  SynBitAnd e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then if l1 == l2
                                                                                  then Right l1
                                                                                  else Left "Operator '&' expects operands of equal types."
                                                                          else Left "Operator '&' expects operands of type int."
                                                    else Left "Operator '&' expects operands of type int." 
                  SynBitXor e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then if l1 == l2
                                                                                  then Right l1
                                                                                  else Left "Operator '~' expects operands of equal types."
                                                                          else Left "Operator '~' expects operands of type int."
                                                    else Left "Operator '~' expects operands of type int."  
                  SynBitOr e1 e2 -> case checkExpr st $ ignorepos e1 of
                                      Left msg -> Left msg
                                      Right l1 -> if l1 == ["int"]
                                                    then case checkExpr st $ ignorepos e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then if l1 == l2
                                                                                  then Right l1
                                                                                  else Left "Operator '|' expects operands of equal types."
                                                                          else Left "Operator '|' expects operands of type int."
                                                    else Left "Operator '|' expects operands of type int."
                  SynEQ e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l1 == l2
                                                                        then Right ["bool"]
                                                                        else Left "Operator '==' expects operands of equal types." 
                  SynNEQ e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l1 == l2
                                                                        then Right ["bool"]
                                                                        else Left "Operator '=/=' expects operands of equal types." 
                  SynLT e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right ["bool"]
                                                                                else Left "Operator '<' expects operands of equal types."
                                                                        else Left "Operator '<' expects operands of type int or float."
                                                  else Left "Operator '<' expects operands of type int or float."    
                  SynLE e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right ["bool"]
                                                                                else Left "Operator '<=' expects operands of equal types."
                                                                        else Left "Operator '<=' expects operands of type int or float."
                                                  else Left "Operator '<=' expects operands of type int or float."     
                  SynGT e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right ["bool"]
                                                                                else Left "Operator '>' expects operands of equal types."
                                                                        else Left "Operator '>' expects operands of type int or float."
                                                  else Left "Operator '>' expects operands of type int or float."    
                  SynGE e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["int"] || l1 == ["float"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                        then if l1 == l2
                                                                                then Right ["bool"]
                                                                                else Left "Operator '>=' expects operands of equal types."
                                                                        else Left "Operator '>=' expects operands of type int or float."
                                                  else Left "Operator '>=' expects operands of type int or float."  
                  SynAnd e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["bool"]
                                                                        then Right l1
                                                                        else Left "Operator 'and' expects operands of type bool."
                                                  else Left "Operator 'and' expects operands of type bool."  
                  SynXor e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["bool"]
                                                                        then Right l1
                                                                        else Left "Operator 'xor' expects operands of type bool."
                                                  else Left "Operator 'xor' expects operands of type bool."  
                  SynOr e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 == ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 == ["bool"]
                                                                        then Right l1
                                                                        else Left "Operator 'or' expects operands of type bool."
                                                  else Left "Operator 'or' expects operands of type bool." 
                  SynIndex e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if (head $ words $ head l1) == "mat"
                                                  then if length e2 == 2
                                                          then case checkExpr st $ ignorepos $ head e2 of
                                                            Left msg -> Left msg
                                                            Right l2 -> if l2 == ["int"]
                                                                          then case checkExpr st $ ignorepos $ last e2 of
                                                                                  Left msg -> Left msg
                                                                                  Right l3 -> if l3 == ["int"]
                                                                                                then Right ["float"]
                                                                                                else Left "Index has to by o type int."
                                                                          else Left "Index has to by o type int."
                                                          else Left "Matrices can only be accessed with two indexes."
                                                  else Left "You can only access matrices through indexes." 
                  --always of type mat
                  SynMat e1 -> if length e1 > 0
                                then if (length $ head e1) > 0
                                        then case checkMatrix st e1 (length $ head e1) of
                                                Left msg -> Left msg
                                                Right _ -> let s = "mat " ++ (show $ length e1) ++ " " ++ (show $ length $ head e1) in
                                                              Right $ s : []
                                        else Left "Matrix needs at least one element."
                                else Left "Matrix needs at least one element."

checkMatrix :: SuperTable -> [[LocSynExpr]] -> Int -> Either String String
checkMatrix st [] n = Right ""
checkMatrix st (h:t) n = if length h /= n
                            then Left "Matrix needs all lines with same number of columns."
                            else case checkMatrixLines st h of
                                  Left msg -> Left msg
                                  Right _ -> checkMatrix st t n

checkMatrixLines :: SuperTable -> [LocSynExpr] -> Either String String
checkMatrixLines st [] = Right ""
checkMatrixLines st (h:t) = case checkExpr st $ ignorepos h of
                              Left msg -> Left msg
                              Right l1 -> if l1 == ["float"]
                                            then checkMatrixLines st t
                                            else Left "Fields of a matrix must be of type float."


findFuncRetTypes :: [STEntry] -> String -> Either String [String]
findFuncRetTypes [] s = Left $ "Function not defined: " ++ s
findFuncRetTypes (h:t) s = case h of
                            Function name _ list -> if name == s
                                                        then Right list
                                                        else findFuncRetTypes t s
                            Procedure name _ -> if name == s
                                                    then Left "Can't call a procedure in an attribution (no return value)."
                                                    else findFuncRetTypes t s
                            Variable name _ _ _ -> if name == s
                                                       then Left "Function not defined." 
                                                       else findFuncRetTypes t s


checkFuncParam :: SuperTable -> SynCall -> Either String ()
checkFuncParam st call = do stEntry <- searchSTEntry (fst st) funcName
                            actualParam <- buildExprTypeList st (ignorepos `fmap` (getexprlist $ getArgList call))
                            case stEntry of
                              Function name _ _-> if funcInST (fst st) name actualParam == True
                                                    then return ()
                                                    else fail $ "No such function: " 
                                                                ++ name ++ "(" ++ (show actualParam) ++ ")"
                              _ -> fail $ "Not a function: " ++ (show call)
                         where
                          funcName = (getlabel $ ignorepos $ getFuncId call)

-- BAD! We're duplicating procInST code
funcInST :: [STEntry] -> String -> [String] -> Bool
funcInST [] _ _ = False
funcInST (h:t) name actualP = case h of
                                Function fname formalP _ -> if name == fname && actualP == formalP
                                                              then True
                                                              else searchTheRest
                                _ -> searchTheRest
                              where searchTheRest = funcInST t name actualP
