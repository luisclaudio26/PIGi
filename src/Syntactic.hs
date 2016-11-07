module Syntactic where

import Control.Monad.Identity
import Text.Parsec (eof)
import Text.Parsec.Prim
import Text.Parsec.Expr
import PosParsec
import Lexical

type SynParser a = Parsec [PosLexToken] () (Located a)

-- | Create a SynParser from an lexical test function
syntoken :: Show a => (LexToken -> Maybe a) -> SynParser a
syntoken test =
    token showTok nextPos testTok
        where showTok = show . ignorepos
              nextPos = getpos
              reloc t = fmap $ mklocated . getpos $ t
              testTok t = reloc t . test . ignorepos $ t


-- | Generic syntactic construct, based on lexical token
-- use only with informationless tokens, like {, ( and =
data SynToken = SynToken { getlex :: LexToken } deriving (Show)

-- | SynParser for SynToken, given the original lexical token.
-- For example, to create an '(' parser,
-- > synlex LexLParen
synlex :: LexToken -> SynParser SynToken
synlex lextok = syntoken $
    \t -> if t == lextok then Just (SynToken t) else Nothing


-- | Syntactic construct for identifier
data SynIdent = SynIdent { getlabel :: String } deriving (Show)

-- | SynParser for identifier
synident :: SynParser SynIdent
synident = syntoken $
    \t -> case t of
            (LexIdent s) -> Just (SynIdent s)
            _ -> Nothing


-- | Syntactic construct for integer literal
data SynLitInt = SynLitInt { getint :: Int } deriving (Show)

-- | SynParser for integer literal
synlitint :: SynParser SynLitInt
synlitint = syntoken $
    \t -> case t of
            (LexLitInt i) -> Just (SynLitInt i)
            _ -> Nothing

-- | Syntactic construct for definition
data SynDef = SynDef (Located SynIdent) (Located SynIdent) deriving (Show)

-- | SynParser for definition of variable
syndef :: SynParser SynDef
syndef = locate $ 
  do synlex LexDef
     var <- synident
     synlex LexColon
     vartype <- synident  
     synlex LexSemicolon
     return (SynDef var vartype)

data SynStmt = SynStmtDef (Located SynDef) deriving (Show) 

synstmt :: SynParser SynStmt
synstmt = locate $
  do def <- syndef
     return (SynStmtDef def)

data SynBlock = SynBlock [Located SynStmt] deriving (Show)

synblock :: SynParser SynBlock
synblock = locate $
  do synlex LexLBraces
     stmts <- many synstmt
     synlex LexRBraces
     return (SynBlock stmts)

data SynIf = SynIf (Located SynExpr) (Located SynBlock) deriving (Show)
data SynElse = SynElse (Located SynBlock) deriving (Show)
data SynElseIf = SynElseIf (Located SynExpr) (Located SynBlock) deriving (Show)

synif :: SynParser SynIf
synif = locate $
  do synlex LexIf
     expr <- synexpr
     content <- synblock
     return (SynIf expr content)

synelse :: SynParser SynElse
synelse = locate $
  do synlex LexElse
     content <- synblock
     return (SynElse content)

synelseif :: SynParser SynElseIf
synelseif = locate $
  do synlex LexElse
     synlex LexIf
     expr <- synexpr
     content <- synblock
     return (SynElseIf expr content)

data SynWhile = SynWhile (Located SynExpr) (Located SynBlock) deriving (Show)

synwhile :: SynParser SynWhile
synwhile = locate $
  do synlex LexWhile
     expr <- synexpr
     content <- synblock
     return (SynWhile expr content)

data SynExpr = SynVal (Located SynIdent)
             | SynPlus (Located SynExpr) (Located SynExpr)
             | SynTimes (Located SynExpr) (Located SynExpr)
             deriving (Show)

synexprval :: SynParser SynExpr
synexprval = locate $
    do val <- synident
       return $ SynVal val

synexprop :: LexToken
          -> (Located SynExpr -> Located SynExpr -> SynExpr)
          -> Parsec [PosLexToken] () (Located SynExpr
                                     -> Located SynExpr
                                     -> Located SynExpr)
synexprop opToken constr =
    do tok <- synlex opToken
       return $ \e1 e2 -> mklocated (getpos tok) $ constr e1 e2

synoptable :: OperatorTable [PosLexToken] () Identity (Located SynExpr)
synoptable = [[Infix (synexprop LexPlus SynPlus) AssocLeft],
              [Infix (synexprop LexTimes SynTimes) AssocLeft]]

synexpr :: SynParser SynExpr
synexpr = buildExpressionParser synoptable synexprval


-- !! EVERYTHING BELOW THIS LINE IS WRONG !!

-- | Syntactic construct for module
data SynModule = SynModule [Located SynIf] deriving (Show)

-- | SynParser for whole module
synmodule :: SynParser SynModule
synmodule = locate $
    do ids <- many synif
       eof
       return (SynModule ids)

