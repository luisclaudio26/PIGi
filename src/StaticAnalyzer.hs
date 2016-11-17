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
                        , getFuncMod :: String } deriving (Show)

type SymbolTable = [STEntry]


data Field = Field { getFieldName :: String
                   , getFieldType :: String } deriving (Show)

data UTEntry = StructType { getStructName :: String     -- Type name
                          , getStructFields :: [Field]  -- List of fields defined inside this struct
                          , getStructMod :: String      -- module where it was defined       
                          } 
             | Primitive { getPrimName :: String } deriving (Show)


{-
typeTable :: UserTypeTable
typeTable = [ Primitive "int",
              Primitive "float",
              Primitive "vec3",
              StructType Ponto ...,
              StructType Cachorro ... ] -}

type UserTypeTable = [UTEntry]

-- TODO: We should receive a SynProgram, which is
-- itself a set of SynModules
stFromModule :: SymbolTable -> SynModule -> SymbolTable 
stFromModule st ( SynModule id stmts ) = stFromModStmts st (getlabel $ ignorepos id) stmts 

stFromModStmts :: SymbolTable -> String -> [SynModStmt] -> SymbolTable
stFromModStmts st id [] = st
stFromModStmts st id (h:t) = stFromModStmts newSt id t
                                where newSt = stFromModStmt st id h

stFromModStmt :: SymbolTable -> String -> SynModStmt -> SymbolTable
stFromModStmt st id s = case s of
                            (SynModStruct stct) -> stFromStruct st id (ignorepos stct)
                            (SynModDef def) -> stFromDef st id (ignorepos def)
                            (SynModProc proc) -> stFromProc st id (ignorepos proc)
                            (SynModFunc func) -> stFromFunc st id (ignorepos func)

stFromStruct :: SymbolTable -> String -> SynStruct -> SymbolTable
stFromStruct st modid stct = entry : st
                                where entry = StructType (getlabel $ ignorepos $ getSynStructName stct)
                                                          stFieldsFromTypedIdent stct
                                                          modid

stFieldsFromTypedIdent :: [SynTypedIdent] -> [Field]
stFieldsFromTypedIdent [] = []
stFieldsFromTypedIdent (h:t) = field : (stFieldsFromTypedIdent t)
                                where field = 

stFromDef :: SymbolTable -> String -> SynDef -> SymbolTable
stFromDef st modid (SynDef typedId) = stFromTypedIdentList st modid typedId

stFromProc :: SymbolTable -> String -> SynProc -> SymbolTable
stFromProc st modid proc = st

stFromFunc :: SymbolTable -> String -> SynFunc -> SymbolTable
stFromFunc st modid (SynFunc name formalParam ret block) = entry : st
                                                            where entry = Function (getlabel $ ignorepos $ name)
                                                                                   (buildFuncTypeStr formalParam ret)
                                                                                    modid
buildFuncTypeStr :: [SynTypedIdent] -> [SynTypedIdent] -> String
buildFuncTypeStr formalParam ret = fp ++ "->" ++ rv
                                    where
                                        fp = show (getTypedIdentType `fmap` formalParam)
                                        rv = show (getTypedIdentType `fmap` ret)


-- TODO: Code for entry is to big; maybe we could create some 
-- helper functions to make it smaller.
stFromTypedIdentList :: SymbolTable -> String -> [SynTypedIdent] -> SymbolTable
stFromTypedIdentList st modid [] = st
stFromTypedIdentList st modid (h:t) = stFromTypedIdentList newST modid t
                                        where newST = entry : st
                                              entry = Variable (getlabel $ ignorepos $ getTypedIdentName h) 
                                                               (getlabel $ ignorepos $ getTypedIdentType h) 
                                                                modid 

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
modDummyRule mod = Left $ show (stFromModule [] mod)