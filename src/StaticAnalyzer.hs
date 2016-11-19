module StaticAnalyzer where

import Syntactic
import PosParsec(ignorepos)

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

data UTEntry = StructType { getStructName :: String     -- Type name
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


stFromStruct :: SuperTable -> String -> SynStruct -> Either String SuperTable 
stFromStruct st modid stct = Right st
{-
stFromStruct st modid stct = Right $ entry : st
                                where entry = StructType (getlabel $ ignorepos $ getSynStructName stct)
                                                          (stFieldsFromTypedIdent stct)
                                                          modid -}

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
stFromProc st modid (SynProc name formalParam block) = if isElemUserTypeTable (getlabel $ ignorepos name) (snd st)
                                                            then Left "Name already being used as a type name."
                                                            else Right (syt, (snd st))
                                                                where syt = entry : (fst st)
                                                                      entry = Procedure (getlabel $ ignorepos name)
                                                                                        (buildProcTypeStr formalParam)
                                                                                        modid  

stFromFunc :: SuperTable -> String -> SynFunc -> Either String SuperTable
stFromFunc st modid (SynFunc name formalParam ret block) = if isElemUserTypeTable (getlabel $ ignorepos name) (snd st)
                                                                then Left "Name already being used as a type name."
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
stFromTypedIdentList st modid (h:t) = if isElemUserTypeTable (getlabel $ ignorepos $ getTypedIdentName h) (snd st)
                                        then Left "Name already being used as a type name." 
                                        else stFromTypedIdentList (newST, snd st) modid t
                                                where newST = entry : (fst st)
                                                      entry = Variable (getlabel $ ignorepos $ getTypedIdentName h) 
                                                                       (getlabel $ ignorepos $ getTypedIdentType h) 
                                                                       modid 

isElemUserTypeTable :: String -> [UTEntry] -> Bool
isElemUserTypeTable s [] = False
isElemUserTypeTable s (h:t) = case h of 
                                StructType name _ _ -> if name == s 
                                                        then True
                                                        else isElemUserTypeTable s t 
                                Primitive name -> if name == s
                                                    then True
                                                    else isElemUserTypeTable s t

-----------------------------------------------
--------- Static analyzer for modules --------- TODO: Move this to another file when 
-----------------------------------------------       things get more structured.
-- This applies all the static semantic rules to x.
-- Notice this will apply rules in inverse order, but
-- we assume them to be commutative somehow (that is,
-- order of verification should not be important).
semModule' :: [SynModule -> Either String SynModule] -> SynModule -> Either String SynModule
semModule' [] x = Right x
semModule' (rule:tail) x  = (semModule' tail x) >>= rule

semModule :: SynModule -> Either String SynModule
semModule = semModule' semModuleRules

-- This is a list of all static semantic rules for
-- modules.
semModuleRules :: [SynModule -> Either String SynModule]
semModuleRules = [modDummyRule]

-- Each semantic rule has type SynModule -> Either String SynModule:
-- Right X indicates that SynModule verifies the rule and can be
-- used, Left X indicates something went wrong and X carries an error
-- message.
modDummyRule :: SynModule -> Either String SynModule       -- Dummy rule for tests only
modDummyRule mod = Left $ show (stFromModule ([],typeTable) mod)