module StaticAnalyzer where

import Syntactic
import PosParsec(ignorepos, Located)
import Types
import qualified Data.Text as T

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
                        , getVarLevel :: Int }  -- scope level
             | Function { getFuncName :: String
                        , getFuncArgTypes :: [String]
                        , getFuncRetTypes :: [String] } 
             | Procedure { getProcName :: String
                         , getProcArgTypes :: [String] } deriving (Show)

type SymbolTable = [STEntry]

data Field = Field { getFieldName :: String
                   , getFieldType :: String } deriving (Show)

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
            , Primitive "mat4"]

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
stFromStruct st stct = let n = getlabel $ ignorepos $ getStructName stct in
                                if isElemUserTypeTable n (snd st) || isElemSymbolTable n 0 (fst st)
                                  then Left "Name already being used as a type name or variable name."
                                  else Right (fst st, newTypeTable)
                                      where newTypeTable = newEntry : (snd st)
                                            newEntry = StructEntry (getlabel $ ignorepos $ getStructName stct)
                                                                   ([])

stFromDef :: SuperTable -> Int -> SynDef -> Either String SuperTable
stFromDef st lvl (SynDef typedId) = stFromTypedIdentList st lvl typedId

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

-- TODO: Code for entry is to big; maybe we could create some 
-- helper functions to make it smaller.
stFromTypedIdentList :: SuperTable -> Int -> [SynTypedIdent] -> Either String SuperTable
stFromTypedIdentList st lvl [] = Right st
stFromTypedIdentList st lvl (h:t) = let name = getlabel $ ignorepos $ getTypedIdentName h in
                                        if isElemUserTypeTable name (snd st) || isElemSymbolTable name lvl (fst st)
                                            then Left "Name already being used as a type name or variable name." 
                                            else stFromTypedIdentList (newST, snd st) lvl t
                                                  where newST = entry : (fst st)
                                                        entry = Variable (getlabel $ ignorepos $ getTypedIdentName h) 
                                                                         (getLabelFromType $ ignorepos $ getTypedIdentType h)
                                                                          lvl

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
                                  Variable name _ varlvl -> if name == s && lvl == varlvl
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

{- DROPPED. This is not useful anymore, as certainly
    we'll have only one rule for modules. This idea for
    chaining rules is "good", though, so I'll leave it
    here so I can copy/paste it someday.

-- This applies all the static semantic rules to x.
-- Notice this will apply rules in inverse order, but
-- we assume them to be commutative somehow (that is,
-- order of verification should not be important).
semModule' :: [SynModule -> Either String SynModule] -> SynModule -> Either String SynModule
semModule' [] x = Right x
semModule' (rule:tail) x  = (semModule' tail x) >>= rule -}

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
checkMod sm@(SynModule id stmts) = case stFromModule ([], typeTable) sm of
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
checkFunc st func = case checkBlock st 1 $ getFuncBlock unlocFunc of
                        Right _ -> Right unlocFunc
                        Left msg -> Left msg
                      where unlocFunc = ignorepos func

checkProc :: SuperTable -> Located SynProc -> Either String SynProc
checkProc st proc = case checkBlock newSt lvl $ getProcBlock unlocProc of
                    Right _ -> Right unlocProc
                    Left msg -> Left msg
                  where lvl = 1
                        unlocProc = ignorepos proc
                        newSt = case stFromTypedIdentList st lvl (getProcArgs $ ignorepos proc) of
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
                      SynStmtIf si -> (Right s, st)
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
checkCall st lvl sc = if ret == True
                        then if formal == actual 
                                then Right sc 
                                else Left ("Parameters type in call '" ++ 
                                              (show sc) ++ 
                                              "' do not match with parameters in definition.")
                        else Left $ (show sc) ++ " is not a procedure."
                      where
                        stEntry = searchSTEntry (fst st) (getlabel $ ignorepos $ getFuncId sc)
                        formal = case stEntry of
                                  Left msg -> Nothing
                                  Right l -> Just $ getProcArgTypes l 
                        actual = case buildExprTypeList st (ignorepos `fmap` (getexprlist $ getArgList sc)) of
                                  Left msg -> Nothing
                                  Right l -> Just l
                        ret = case stEntry of
                                  Left msg -> False
                                  Right (Procedure _ _ ) -> True
                                  Right (Function _ _ _) -> False 

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
                                      else Left $ "Not a bool expression in 'while " ++ (show $ getWhileCondition sw) ++ "'"
                        where
                          exprOk = checkExpr st (ignorepos $ getWhileCondition sw)
                          blockOk = checkBlock st (lvl+1) (getWhileBlock sw)

checkAttr :: SuperTable -> SynAttr -> Either String SynAttr
checkAttr st sa@(SynAttr si se) = case buildIdentTypeList st (ignorepos `fmap` si) of
                                    Left msg -> Left msg
                                    Right s1 -> case buildExprTypeList st (ignorepos `fmap` se) of
                                                  Left msg -> Left msg
                                                  Right s2 -> if typeListsEqual s1 s2
                                                                then Right sa
                                                                else Left "Variable and expression have different types in attribution."

typeListsEqual :: [String] -> [String] -> Bool
typeListsEqual [] [] = True
typeListsEqual (h1:t1) (h2:t2) = if h1 /= h2 && h1 /= "_"
                                    then False
                                    else typeListsEqual t1 t2
           
buildIdentTypeList :: SuperTable -> [SynIdent] -> Either String [String]
buildIdentTypeList st [] = Right []
buildIdentTypeList st (h:t) = case identType st h of
                                Left msg -> Left msg
                                Right s -> case buildIdentTypeList st t of
                                            Left msg -> Left msg
                                            Right l -> Right $ s : l

buildExprTypeList :: SuperTable -> [SynExpr] -> Either String [String]
buildExprTypeList st [] = Right []
buildExprTypeList st (h:t) = case checkExpr st h of
                                Left msg -> Left msg
                                Right s -> case buildExprTypeList st t of
                                            Left msg -> Left msg
                                            Right l -> Right $ s ++ l

identType :: SuperTable -> SynIdent -> Either String String
identType (syt, utt) si@(SynIdent s) = if s == "_"
                                          then Right s
                                          else case searchSymbolTable syt s of
                                            Right vtype -> Right vtype
                                            Left _ -> case searchUserTypeTable utt s of
                                                        Right ftype -> Right ftype
                                                        Left msg -> Left msg 

-- PENDING !!! [LUÍS] Verificar o nível do símbolo ao buscar
searchSymbolTable :: [STEntry] -> String -> Either String String
searchSymbolTable [] s = Left "Variable not defined."
searchSymbolTable (h:t) s = case h of 
                                Variable name vtype _ -> if name == s
                                                          then Right vtype
                                                          else searchSymbolTable t s
                                _ -> searchSymbolTable t s

searchSTEntry :: [STEntry] -> String -> Either String STEntry
searchSTEntry [] s = Left $ "Symbol \"" ++ s ++ "\" was not defined."
searchSTEntry (h:t) s = case h of
                          Variable name _ _ -> if name == s
                                                then Right h
                                                else searchSTEntry t s
                          Function name _ _ -> if name == s
                                                then Right h
                                                else searchSTEntry t s
                          Procedure name _ -> if name == s
                                                then Right h
                                                else searchSTEntry t s

searchUserTypeTable :: [UTEntry] -> String -> Either String String
searchUserTypeTable [] s = Left "Variable not defined."
searchUserTypeTable (h:t) s = case h of
                                StructEntry name _ -> if name == s
                                                          then Right $ "struct " ++ name
                                                          else searchUserTypeTable t s
                                _ -> searchUserTypeTable t s

searchStructField :: [UTEntry] -> String -> String -> Either String [String]
searchStructField [] struct field = Left "Variable not defined."
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
                  SynLitIntExpr _ -> Right $ "int" : []
                  SynLitFloatExpr _ -> Right $ "float" : []
                  SynLitBoolExpr _ -> Right $ "bool" : []
                  SynCallExpr e -> case findFuncRetTypes (fst st) $ getlabel $ ignorepos $ getFuncId $ ignorepos e of
                                      Left msg -> Left msg
                                      Right ftype -> Right ftype
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
                                      Right l1 -> let s1 = head l1 in
                                                    if T.unpack (T.take 6 (T.pack s1))  == "struct"
                                                      then searchStructField (snd st) (T.unpack (T.take (length s1 - 7) (T.pack s1))) $ getlabel $ ignorepos e2
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
                                                                                else Left "If the first operand is of type int or float, operator '*' expects second operator of same type."
                                                                        else Left "If the first operand is of type int or float, operator '*' expects second operator of type int or float."
                                                  else if l1 == ["vec"] || l1 == ["vec2"] || l1 == ["vec3"] || l1 == ["vec4"] 
                                                          then case checkExpr st $ ignorepos e2 of
                                                                  Left msg -> Left msg
                                                                  Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                                then Right l1
                                                                                else if l2 == ["mat"] || l2 == ["mat2"] || l2 == ["mat3"] || l2 == ["mat4"]
                                                                                        then Right ["vec"]
                                                                                        else Left "Operands of wrong types."
                                                          else if l1 /= ["bool"]
                                                                  then case checkExpr st $ ignorepos e2 of
                                                                        Left msg -> Left msg
                                                                        Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                                      then Right l1
                                                                                      else if l2 == ["vec"] || l2 == ["vec2"] || l2 == ["vec3"] || l2 == ["vec4"] 
                                                                                              then Right ["mat"]
                                                                                              else Left "Operands of wrong types."
                                                                  else Left "Operator '*' expects operands of type int, float, vec or mat."
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
                                                  else if l1 /= ["bool"]
                                                          then case checkExpr st $ ignorepos e2 of
                                                                  Left msg -> Left msg
                                                                  Right l2 -> if l2 == ["int"] || l2 == ["float"]
                                                                                then Right l1 
                                                                                else Left "If the first operand is of type vec or mat, operator '/' expects second operator of type int or float."
                                                          else Left "Operator '/' expects operands of type int, float, vec or mat."
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
                                                                        else Left "Operator '+' expects operands of type int, float, vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                                                  else Left "Operator '+' expects operands of type int, float, vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                  SynMinus e1 e2 -> case checkExpr st $ ignorepos e1 of
                                    Left msg -> Left msg
                                    Right l1 -> if l1 /= ["bool"]
                                                  then case checkExpr st $ ignorepos e2 of
                                                          Left msg -> Left msg
                                                          Right l2 -> if l2 /= ["bool"]
                                                                        then if l1 == l2
                                                                                then Right l1
                                                                                else Left "Operator '-' (binary) expects operands of equal types."
                                                                        else Left "Operator '-' (binary) expects operands of type int, float, vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                                                  else Left "Operator '-' (binary) expects operands of type int, float, vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                  SynDotTimes e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if l1 /= ["int"] && l1 /= ["float"] && l1 /= ["bool"]
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if l2 /= ["int"] && l2 /= ["float"] && l2 /= ["bool"]
                                                                            then if l1 == l2
                                                                                    then Right l1
                                                                                    else Left "Operator '.*.' expects operands of equal types."
                                                                            else Left "Operator '.*.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                                                      else Left "Operator '.*.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                  SynDotDiv e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if l1 /= ["int"] && l1 /= ["float"] && l1 /= ["bool"]
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if l2 /= ["int"] && l2 /= ["float"] && l2 /= ["bool"]
                                                                            then if l1 == l2
                                                                                    then Right l1
                                                                                    else Left "Operator './.' expects operands of equal types."
                                                                            else Left "Operator './.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                                                      else Left "Operator './.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                  SynDot e1 e2 -> case checkExpr st $ ignorepos e1 of
                                        Left msg -> Left msg
                                        Right l1 -> if l1 /= ["int"] && l1 /= ["float"] && l1 /= ["bool"]
                                                      then case checkExpr st $ ignorepos e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if l2 /= ["int"] && l2 /= ["float"] && l2 /= ["bool"]
                                                                            then if l1 == l2
                                                                                    then Right l1
                                                                                    else Left "Operator '.' expects operands of equal types."
                                                                            else Left "Operator '.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
                                                      else Left "Operator '.' expects operands of type vec, vec2, vec3, vec4, mat, mat2, mat3 or mat4."
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
                                    Right l1 -> if l1 == ["vec"] || l1 == ["vec2"] || l1 == ["vec3"] || l1 == ["vec4"]
                                                  then if length e2 == 1
                                                        then case checkExpr st $ ignorepos $ head e2 of
                                                              Left msg -> Left msg
                                                              Right l2 -> if l2 == ["int"]
                                                                            then Right ["float"]
                                                                            else Left "Index has to by o type int."
                                                        else Left "Vectors can only be accessed with one index."
                                                  else if l1 == ["mat"] || l1 == ["mat2"] || l1 == ["mat3"] || l1 == ["mat4"]
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
                                                          else Left "You need a vector or matrix to access through indexes." 
                  --always of type mat
                  SynMat e1 -> if length e1 > 0
                                then if (length $ head e1) > 0
                                        then case checkMatrix st e1 (length $ head e1) of
                                                Left msg -> Left msg
                                                Right _ -> Right ["mat"]
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
findFuncRetTypes [] s = Left "Function not defined."
findFuncRetTypes (h:t) s = case h of
                            Function name list _ -> if name == s
                                                        then Right list
                                                        else findFuncRetTypes t s
                            Procedure name _ -> if name == s
                                                    then Left "Can't call a procedure in an attribution (no return value)."
                                                    else findFuncRetTypes t s
                            Variable name _ _ -> if name == s
                                                    then Left "Function not defined." 
                                                    else findFuncRetTypes t s




