module Syntactic where

import Control.Monad.Identity
import Text.Parsec (eof)
import Text.Parsec.Prim
import Text.Parsec.Expr
import PosParsec
import Lexical

type SynSpecParser a = Parsec [PosLexToken] () a
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
data SynIdent = SynIdent { getlabel :: String } -- deriving (Show)

instance Show SynIdent where
    show x = getlabel x

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


-- | Syntactic construct for expressions
data SynExpr = SynVal (Located SynIdent)
             | SynPar      LocSynExpr
             | SynExp      LocSynExpr LocSynExpr
             | SynNeg      LocSynExpr
             | SynBitNot   LocSynExpr
             | SynTimes    LocSynExpr LocSynExpr
             | SynDiv      LocSynExpr LocSynExpr
             | SynDotTimes LocSynExpr LocSynExpr
             | SynDotDiv   LocSynExpr LocSynExpr
             | SynMod      LocSynExpr LocSynExpr
             | SynDot      LocSynExpr LocSynExpr 
             | SynPlus     LocSynExpr LocSynExpr
             | SynMinus    LocSynExpr LocSynExpr
             | SynRShift   LocSynExpr LocSynExpr
             | SynLShift   LocSynExpr LocSynExpr
             | SynEQ       LocSynExpr LocSynExpr
             | SynNEQ      LocSynExpr LocSynExpr
             | SynLT       LocSynExpr LocSynExpr
             | SynLE       LocSynExpr LocSynExpr
             | SynGT       LocSynExpr LocSynExpr
             | SynGE       LocSynExpr LocSynExpr
             | SynBitAnd   LocSynExpr LocSynExpr
             | SynBitXor   LocSynExpr LocSynExpr
             | SynBitOr    LocSynExpr LocSynExpr
             | SynNot      LocSynExpr
             | SynAnd      LocSynExpr LocSynExpr
             | SynXor      LocSynExpr LocSynExpr
             | SynOr       LocSynExpr LocSynExpr

paren :: (Show a) => a -> String
paren x = "(" ++ show x ++ ")"

parenBin :: (Show a, Show b) => String -> a -> b -> String
parenBin op x y = "(" ++ show x ++ op ++ show y ++ ")"

parenUn :: (Show a) => String -> a -> String
parenUn op x = "(" ++ op ++ show x ++ ")"

instance Show SynExpr where
    show (SynPar x)        = paren x
    show (SynExp x y)      = parenBin "^" x y
    show (SynNeg x)        = parenUn "-" x
    show (SynBitNot x)     = parenUn "!" x
    show (SynTimes x y)    = parenBin "*" x y
    show (SynDiv x y)      = parenBin "/" x y
    show (SynDotTimes x y) = parenBin ".*." x y
    show (SynDotDiv x y)   = parenBin "./." x y
    show (SynMod x y)      = parenBin " mod " x y
    show (SynDot x y)      = parenBin "." x y
    show (SynPlus x y)     = parenBin "+" x y
    show (SynMinus x y)    = parenBin "-" x y
    show (SynRShift x y)   = parenBin ">>" x y
    show (SynLShift x y)   = parenBin "<<" x y
    show (SynEQ x y)       = parenBin "==" x y
    show (SynNEQ x y)      = parenBin "=/=" x y
    show (SynLT x y)       = parenBin "<" x y
    show (SynLE x y)       = parenBin "<=" x y
    show (SynGT x y)       = parenBin ">" x y
    show (SynGE x y)       = parenBin ">=" x y
    show (SynBitAnd x y)   = parenBin "&" x y
    show (SynBitXor x y)   = parenBin "~" x y
    show (SynBitOr x y)    = parenBin "|" x y
    show (SynNot x)        = parenUn "not " x
    show (SynAnd x y)      = parenBin " and " x y
    show (SynXor x y)      = parenBin " xor " x y
    show (SynOr x y)       = parenBin " or " x y
    show (SynVal id)       = show id

type LocSynExpr = Located SynExpr

-- | Value expression syntactic parser
synexprVal :: SynParser SynExpr
synexprVal = locate $
    do val <- synident
       return $ SynVal val

-- | Parenthesized expression syntactic parser
synexprPar :: SynParser SynExpr
synexprPar = locate $
    do synlex LexLParen
       expr <- synexpr
       synlex LexRParen
       return $ SynPar expr

-- | High precedence expression unit parser
synexprUnit :: SynParser SynExpr
synexprUnit = synexprPar <|> synexprVal

-- | Binary operator syntactic parser
synexprBinOp :: LexToken -- ^operator lexical token
             -> (LocSynExpr -> LocSynExpr -> SynExpr) -- ^ binary expression constructor
             -> Parsec [PosLexToken] () (LocSynExpr
                                        -> LocSynExpr
                                        -> LocSynExpr)
synexprBinOp opToken constr =
    do tok <- synlex opToken
       return $ \e1 e2 -> mklocated (getpos tok) $ constr e1 e2

-- | Unary operation syntactic parser
synexprUnOp :: LexToken -- ^operator lexical token
            -> (LocSynExpr -> SynExpr) -- ^ unary expression constructor
            -> SynSpecParser (LocSynExpr -> LocSynExpr)
synexprUnOp opToken constr =
    do tok <- synlex opToken
       return $ \e -> mklocated (getpos tok) $ constr e


-- | Builds an binary left associative operator
opBinL :: LexToken -- ^operator lexical token
       -> (LocSynExpr -> LocSynExpr -> SynExpr) -- ^ binary expression constructor
       -> Operator [PosLexToken] () Identity LocSynExpr
opBinL ltok stok = Infix (synexprBinOp ltok stok) AssocLeft


-- | Builds an binary right associative operator
opBinR :: LexToken -- ^operator lexical token
       -> (LocSynExpr -> LocSynExpr -> SynExpr) -- ^ binary expression constructor
       -> Operator [PosLexToken] () Identity LocSynExpr
opBinR ltok stok = Infix (synexprBinOp ltok stok) AssocRight


-- | Builds an unary operator
opUnL :: LexToken -- ^ operator lexical token
      -> (Located SynExpr -> SynExpr) -- ^ unary expression constructor
      -> Operator [PosLexToken] () Identity LocSynExpr
opUnL ltok stok = Prefix (synexprUnOp ltok stok)


-- | Operator table, by precedence order
synoptable :: OperatorTable [PosLexToken] () Identity LocSynExpr
synoptable = [ -- highest precedence
              [ opBinR LexExp SynExp ],
              [ opUnL LexMinus SynNeg
              , opUnL LexBitNot SynBitNot ],
              [ opBinL LexTimes SynTimes
              , opBinL LexDiv SynDiv
              , opBinL LexDotTimes SynDotTimes
              , opBinL LexDotDiv SynDotDiv
              , opBinL LexMod SynMod ],
              [ opBinL LexDot SynDot ],
              [ opBinL LexPlus SynPlus
              , opBinL LexMinus SynMinus ],
              [ opBinL LexLShift SynLShift 
              , opBinL LexRShift SynRShift ],
              [ opBinL LexEQ SynEQ
              , opBinL LexNEQ SynNEQ
              , opBinL LexLT SynLT
              , opBinL LexLE SynLE
              , opBinL LexGT SynGT
              , opBinL LexGE SynGE ],
              [ opBinL LexBitAnd SynBitAnd ],
              [ opBinL LexBitXor SynBitXor ],
              [ opBinL LexBitOr SynBitOr ],
              [ opUnL LexNot SynNot ],
              [ opBinL LexAnd SynAnd ],
              [ opBinL LexXor SynXor ],
              [ opBinL LexOr SynOr ]
             ] -- lowest precedence


-- | Expression syntactic parser
synexpr :: SynParser SynExpr
synexpr = buildExpressionParser synoptable synexprUnit

-- !! EVERYTHING BELOW THIS LINE IS WRONG !!

-- | Syntactic construct for module
data SynModule = SynModule [Located SynExpr] deriving (Show)

-- | SynParser for whole module
synmodule :: SynParser SynModule
synmodule = locate $
    do ids <- many synexpr
       eof
       return (SynModule ids)

