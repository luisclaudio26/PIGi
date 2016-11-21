module StaticAnalyzer where

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
                        , getVarMod :: String }  -- module where it was defined 
             | Function { getFuncName :: String
                        , getFuncType :: String
                        , getFuncMod :: String } 
             | Procedure { getProcName :: String
                         , getProcType :: String
                         , getProcMod :: String} deriving (Show)

type SymbolTable = [STEntry]

data Field = Field { getFieldName :: String
                   , getFieldType :: String } deriving (Show)

data UTEntry = StructEntry { getStructName :: String     -- Type name
                          , getStructFields :: [Field]  -- List of fields defined inside this struct
                          , getStructMod :: String }     -- module where it was defined
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
stFromModule st ( SynModule id stmts ) = stFromModStmts st (getlabel $ ignorepos id) stmts 

stFromModStmts :: SuperTable -> String -> [SynModStmt] -> Either String SuperTable
stFromModStmts st id [] = Right st
stFromModStmts st id (h:t) = let result = stFromModStmt st id h in
                             case result of
                                Left errorMsg  -> Left errorMsg
                                Right newSt -> stFromModStmts newSt id t


stFromModStmt :: SuperTable -> String -> SynModStmt -> Either String SuperTable
stFromModStmt st id s = case s of
                            (SynModStruct stct) -> stFromStruct st id (ignorepos stct)
                            (SynModDef def) -> stFromDef st id (ignorepos def)
                            (SynModProc proc) -> stFromProc st id (ignorepos proc)
                            (SynModFunc func) -> stFromFunc st id (ignorepos func)


-- TODO: We can still define a struct with a name that was already used by
-- a variable. Check that after!
stFromStruct :: SuperTable -> String -> SynStruct -> Either String SuperTable 
stFromStruct st modid stct = let n = getlabel $ ignorepos $ getSynStructName stct in
                                if isElemUserTypeTable n modid (snd st) || isElemSymbolTable n modid (fst st)
                                  then Left "Name already being used as a type name or variable name."
                                  else Right (fst st, newTypeTable)
                                      where newTypeTable = newEntry : (snd st)
                                            newEntry = StructEntry (getlabel $ ignorepos $ getSynStructName stct)
                                                                   ([])
                                                                   modid

{-
stFieldsFromTypedIdent :: [SynTypedIdent] -> [Field]
stFieldsFromTypedIdent [] = []
stFieldsFromTypedIdent (h:t) = field : (stFieldsFromTypedIdent t)
                                where field = Field (getlabel $ ignorepos getTypedIdentName h)
                                                    (getTypedIdentType h)
-}

stFromDef :: SuperTable -> String -> SynDef -> Either String SuperTable
stFromDef st modid (SynDef typedId) = stFromTypedIdentList st modid typedId

stFromProc :: SuperTable -> String -> SynProc -> Either String SuperTable
stFromProc st modid (SynProc name formalParam block) = let n = getlabel $ ignorepos name in
                                                          if isElemUserTypeTable n modid (snd st) || isElemSymbolTable n modid (fst st)
                                                            then Left "Name already being used as a type name or variable name."
                                                            else Right (syt, (snd st))
                                                                where syt = entry : (fst st)
                                                                      entry = Procedure (getlabel $ ignorepos name)
                                                                                        (buildProcTypeStr formalParam)
                                                                                        modid  

stFromFunc :: SuperTable -> String -> SynFunc -> Either String SuperTable
stFromFunc st modid (SynFunc name formalParam ret block) = let n = getlabel $ ignorepos name in
                                                              if isElemUserTypeTable n modid (snd st) || isElemSymbolTable n modid (fst st)
                                                                then Left "Name already being used as a type name or variable name."
                                                                else Right (syt, (snd st))
                                                                        where syt = entry : (fst st)
                                                                              entry = Function (getlabel $ ignorepos name)
                                                                                               (buildFuncTypeStr formalParam ret)
                                                                                               modid

buildFuncTypeStr :: [SynTypedIdent] -> [SynTypedIdent] -> String
buildFuncTypeStr formalParam ret = fp ++ "->" ++ rv
                                    where
                                        fp = show (getTypedIdentType `fmap` formalParam)
                                        rv = show (getTypedIdentType `fmap` ret)

buildProcTypeStr :: [SynTypedIdent] -> String
buildProcTypeStr formalParam = show (getTypedIdentType `fmap` formalParam)



-- TODO: Code for entry is to big; maybe we could create some 
-- helper functions to make it smaller.
stFromTypedIdentList :: SuperTable -> String -> [SynTypedIdent] -> Either String SuperTable
stFromTypedIdentList st modid [] = Right st
stFromTypedIdentList st modid (h:t) = let name = getlabel $ ignorepos $ getTypedIdentName h in
                                        if isElemUserTypeTable name modid (snd st) || isElemSymbolTable name modid (fst st)
                                            then Left "Name already being used as a type name or variable name." 
                                            else stFromTypedIdentList (newST, snd st) modid t
                                                  where newST = entry : (fst st)
                                                        entry = Variable (getlabel $ ignorepos $ getTypedIdentName h) 
                                                                         (getLabelFromType $ ignorepos $ getTypedIdentType h) 
                                                                         modid 

isElemUserTypeTable :: String -> String -> [UTEntry] -> Bool
isElemUserTypeTable s m [] = False
isElemUserTypeTable s m (h:t) = case h of 
                                StructEntry name _ modid -> if modid == m && name == s 
                                                              then True
                                                              else isElemUserTypeTable s m t 
                                Primitive name -> if name == s
                                                    then True
                                                    else isElemUserTypeTable s m t

isElemSymbolTable :: String -> String -> [STEntry] -> Bool
isElemSymbolTable s m [] = False
isElemSymbolTable s m (h:t) = case h of 
                                Variable name _ modid -> if modid == m && name == s 
                                                          then True
                                                          else isElemSymbolTable s m t 
                                Function name _ modid -> if modid == m && name == s
                                                          then True
                                                          else isElemSymbolTable s m t
                                Procedure name _ modid -> if modid == m && name == s
                                                            then True
                                                            else isElemSymbolTable s m t

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
checkMod (SynModule id stmts) = case checkModStmts stmts of
                                  Right _ -> Right (SynModule id stmts)
                                  Left msg -> Left msg

checkModStmts :: [SynModStmt] -> Either String [SynModStmt]
checkModStmts [] = Right []
checkModStmts (stmt:tail) = case checkedStmt of
                            Left msg -> Left msg
                            Right _ -> do x <- checkedStmt
                                          y <- checkModStmts tail
                                          Right (x : y)
                          where checkedStmt = checkModStmt stmt

checkModStmt :: SynModStmt -> Either String SynModStmt
checkModStmt stmt = case stmt of
                  SynModStruct ss -> Right stmt    -- No rule for structs nor def's (so far)
                  SynModDef sd -> Right stmt
                  SynModProc sp -> case checkProc sp of
                                      Right _ -> Right stmt
                                      Left msg -> Left msg
                  SynModFunc sf -> checkFunc sf

checkFunc :: Located SynFunc -> Either String SynModStmt -- PENDING
checkFunc func = Right $ SynModFunc func 

checkProc :: Located SynProc -> Either String SynProc
checkProc proc = case checkBlock $ getProcBlock unlocProc of
                    Right _ -> Right unlocProc
                    Left msg -> Left msg
                  where unlocProc = ignorepos proc

checkBlock :: Located SynBlock -> Either String SynBlock
checkBlock block = case checkedStmts of
                      Right _ -> Right $ ignorepos block
                      Left msg -> Left msg
                    where checkedStmts = checkStmts $ ignorepos `fmap` getStmts (ignorepos block)

checkStmts :: [SynStmt] -> Either String [SynStmt]
checkStmts [] = Right []
checkStmts (stmt:tail) = case checkedStmt of
                            Left msg -> Left msg
                            Right _ -> do h <- checkedStmt
                                          t <- checkStmts tail
                                          Right (h:t)
                          where checkedStmt = checkStmt stmt

checkStmt :: SynStmt -> Either String SynStmt -- PENDING
checkStmt s = case s of
                SynStmtDef sd -> Right s
                SynStmtAttr sa -> Left "TODO: Check attributions."
                SynStmtDefAttr sda -> Right s
                SynStmtIf si -> Right s
                SynStmtWhile sw -> Right s
                SynStmtCall sc -> Right s