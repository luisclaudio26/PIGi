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
data STEntry = STEntry { getId :: String    -- identifier
                       , getType :: String  -- type
                       , getMod :: String   -- module where it was defined
                        } deriving (Show)
type SymbolTable = [STEntry]

-- TODO: We should receive a SynProgram, which is
-- itself a set of SynModules
stFromModule :: SymbolTable -> SynModule -> SymbolTable 
stFromModule st ( SynModule id stmts ) = stFromModStmts st (getlabel $ ignorepos id) stmts 

stFromModStmts :: SymbolTable -> String -> [SynModStmt] -> SymbolTable
stFromModStmts st id [] = st
stFromModStmts st id (h:t) = stFromModStmts newSt id t
                                where newSt = stFromModStmt st id h

stFromModStmt :: SymbolTable -> String -> SynModStmt -> SymbolTable
stFromModStmt st id s = st

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
modDummyRule :: SynModule -> Either String SynModule
modDummyRule mod = Right mod
--modDummyRule mod = Left "Bizarre error!"