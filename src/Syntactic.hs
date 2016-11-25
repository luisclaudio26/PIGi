module Syntactic where

import Control.Monad.Identity
import Data.List
import Data.Maybe
import Text.Parsec (eof, sepBy)
import Text.Parsec.Prim
import Text.Parsec.Expr
import Types
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
data SynIdent = SynIdent { getlabel :: String }

instance Show SynIdent where
    show x = getlabel x

instance Named SynIdent where
    getName = getlabel

-- | SynParser for identifier
synident :: SynParser SynIdent
synident = syntoken $
    \t -> case t of
            (LexIdent s) -> Just (SynIdent s)
            _ -> Nothing

{--This aux function it's temporary.--}
getLabelFromType :: SynType -> String
getLabelFromType t = getlabel . ignorepos . getTypeIdent $ t

data SynAnnotation = SynAnnotation { getModifier :: Maybe (Located SynToken) 
                                   , getRef :: Maybe (Located SynToken)
                                   } deriving (Show)

synannotation :: SynParser SynAnnotation
synannotation = locate $
    do tok <- fmap Just (synlex LexModMut)
          <|> fmap Just (synlex LexModConst)
          <|> return Nothing
       ra <- fmap Just (synlex LexRef) <|> return Nothing
       return $ SynAnnotation tok ra

-- | Syntactic construct for type 
data SynType = SynTypeNGen{ getAnnotation :: (Maybe(Located SynAnnotation))
                          , getTypeIdent :: (Located SynIdent)}
             | SynTypeGen { getAnnotation :: (Maybe(Located SynAnnotation))
                          , getTypeIdent :: (Located SynIdent)
                          , getTypeArgs  :: (Located SynTypeList) }deriving (Show)

instance Typed SynType where
    toType tp = strToType . getName . getTypeIdent $ tp

instance AnnotatedTyped SynType where
    toAnnType tp = AnnotatedType (toType tp) anns
        where
            msynann = getAnnotation tp
            anns = case msynann of
                     Nothing -> []
                     Just locsynann ->
                         let synann = ignorepos locsynann
                          in if isNothing (getRef synann)
                             then []
                             else [Ref]


-- | SynParser of type
syntype :: SynParser SynType
syntype = try syntypegen <|> syntypengen


-- | SynParser of a non generic type
syntypengen:: SynParser SynType
syntypengen = locate $
  do extension <- fmap Just synannotation <|> return Nothing
     name <- synident
     return $ SynTypeNGen extension name

-- | SynParser of a generic type. 
-- Example: struct<int, float>
syntypegen:: SynParser SynType
syntypegen = locate $
  do extension <- fmap Just synannotation <|> return Nothing
     name <- synident
     args <- synttype
     return $ SynTypeGen extension name args

-- | Syntactic construct for list of syntype 
data SynTypeList = SynTypeList { gettypelist :: [Located SynType] } deriving (Show)

syntypelist :: SynSpecParser SynTypeList
syntypelist = fmap SynTypeList (syntype `sepBy` (synlex LexComma))

-- <t1, t2, t3>
syntident :: SynParser SynIdentList
syntident = locate $
  do synlex LexLT
     formalParam <- fmap getidentlist synidentlist
     synlex LexGT
     return $ SynIdentList formalParam

-- <int, float, bool>
synttype :: SynParser SynTypeList
synttype = locate $
  do synlex LexLT
     formalParam <- fmap gettypelist syntypelist
     synlex LexGT
     return $ SynTypeList formalParam

-- | Syntactic construct for identifiers list
data SynIdentList = SynIdentList { getidentlist :: [Located SynIdent] } deriving (Show)

-- | SynParser for list of identifiers
synidentlist :: SynSpecParser SynIdentList
synidentlist = fmap SynIdentList (synident `sepBy` (synlex LexComma))

-- | Syntactic construct for typed identifier
data SynTypedIdent = SynTypedIdent { getTypedIdentName :: (Located SynIdent)
                                   , getTypedIdentType :: (Located SynType) } deriving (Show)

instance Typed SynTypedIdent where
    toType = toType . getTypedIdentType

instance Named SynTypedIdent where
    getName = getName . getTypedIdentName

instance AnnotatedTyped SynTypedIdent where
    toAnnType = toAnnType . getTypedIdentType


synTypedIdentL :: SynSpecParser [SynTypedIdent]
synTypedIdentL = do var <- fmap getidentlist synidentlist
                    synlex LexColon
                    vartype <- syntype
                    let f t i = SynTypedIdent i t
                    return $ fmap (f vartype) var

-- Parsec s u m [SynTypedIdent]

-- | Syntactic construct for typed identifier list
data SynTypedIdentList = SynTypedIdentList { gettypedidentlist :: [SynTypedIdent] }


instance TypedList SynTypedIdentList where
  toTypeList = map toType . gettypedidentlist
  
-- | SynParser for list of identifiers
synTypedIdentList :: SynSpecParser SynTypedIdentList
synTypedIdentList = fmap (SynTypedIdentList . concat) (synTypedIdentL `sepBy` (synlex LexComma))

-- | Syntactic construct for integer literal
data SynLitInt = SynLitInt { getint :: Int } 

instance Show SynLitInt where
    show = show . getint

-- | SynParser for integer literal
synlitint :: SynParser SynLitInt
synlitint = syntoken $
    \t -> case t of
            (LexLitInt i) -> Just (SynLitInt i)
            _ -> Nothing

-- | Syntactic construct for float literal
data SynLitFloat = SynLitFloat { getfloat :: Float }

instance Show SynLitFloat where
    show = show . getfloat

-- | SynParser for float literal
synlitfloat :: SynParser SynLitFloat
synlitfloat = syntoken $
    \t -> case t of
            (LexLitFloat f) -> Just (SynLitFloat f)
            _ -> Nothing

-- | Syntactic construct for boolean literal
data SynLitBool = SynLitBool { getbool :: Bool }

instance Show SynLitBool where
    show = show . getbool

-- | SynParser for boolean literal
synlitbool :: SynParser SynLitBool
synlitbool = syntoken $
    \t -> case t of
            (LexLitBool b) -> Just (SynLitBool b)
            _ -> Nothing

-- | Syntactic construct for 'struct'
data SynStruct = SynStruct { getTemplateIdent :: (Maybe (Located SynIdentList))
                            ,getStructName :: (Located SynIdent)
                            ,getTuple :: [SynTypedIdent]
                           } deriving (Show)

-- | SynParser for 'struct'
synstruct :: SynParser SynStruct
synstruct = locate $
  do synlex LexStruct
     formalParam <- fmap Just syntident <|> return Nothing
     name <- synident
     synlex LexAttr
     synlex LexLParen
     i <- fmap gettypedidentlist synTypedIdentList 
     synlex LexRParen
     synlex LexSemicolon
     return $ SynStruct formalParam name i

instance Typed SynStruct where
    toType (SynStruct _ n tis) = StructType (getName n) (map extr tis)
        where extr ti = (getName ti, toType ti)

instance Named SynStruct where
    getName (SynStruct _ n _) = getName n

-- = Definitions

-- | Syntactic construct for definition
data SynDef = SynDef { getDefTypedIdents :: [SynTypedIdent]
                     } deriving (Show)

-- | SynParser for definition of variable
syndef :: SynParser SynDef
syndef = locate $ 
  do synlex LexDef
     vartyped <- synTypedIdentL
     synlex LexSemicolon
     return $ SynDef vartyped
-- = Attribution

-- == Simple attribution 

-- | Syntactic construct for attribution 
data SynAttr = SynAttr { getAttrVars :: [Located SynIdent]
                       , getAttrExprs :: [Located SynExpr]
                       } deriving (Show)

-- | SynParser for attribution
-- TODO: accept to struct. Example: `p = (4.0, 2.0, 1.0);`
synattr :: SynParser SynAttr
synattr = locate $
  do var <- fmap getidentlist synidentlist
     stok <- synlex LexAttr
     value <- fmap getexprlist synexprlist
     synlex LexSemicolon
     return $ SynAttr var value

     -- | SynParser for attribution
synopattr :: SynParser SynAttr
synopattr = locate $
  do var <- synident
     stok <- synlex LexPlusAttr <|> synlex LexMinusAttr <|> synlex LexTimesAttr <|> synlex LexDivAttr
     expr <- synexpr
     synlex LexSemicolon
     return $ SynAttr [var] $
       let varexpr =  mklocated (getpos var) (SynIdentExpr var)
           stokPos = mklocated (getpos stok)
        in case ignorepos stok of 
             (SynToken LexPlusAttr)  ->
                 [stokPos (SynPlus varexpr expr)]
             (SynToken LexMinusAttr) ->
                 [stokPos (SynMinus varexpr expr)]
             (SynToken LexTimesAttr) ->
                 [stokPos (SynTimes varexpr expr)]
             (SynToken LexDivAttr)   ->
                 [stokPos(SynDiv varexpr expr)]

-- = Definition and attribution
-- | Syntactic construct for definition & attribution
data SynDefAttr = SynDefAttr [SynTypedIdent] [Located SynExpr] deriving (Show)

-- | SynParser for definition & attribution
syndefattr :: SynParser SynDefAttr
syndefattr = locate $
  do synlex LexDef
     vartyped <- synTypedIdentL
     synlex LexAttr
     value <- fmap getexprlist synexprlist
     synlex LexSemicolon
     let f t i = SynTypedIdent i t  
     return $ SynDefAttr vartyped value

-- | Statement syntactic construct
data SynStmt = SynStmtDef (Located SynDef)
             | SynStmtAttr (Located SynAttr)
             | SynStmtDefAttr (Located SynDefAttr)
             | SynStmtIf (Located SynIf)
             | SynStmtFor (Located SynFor)
             | SynStmtWhile (Located SynWhile)
             | SynStmtCall (Located SynCall)
             | SynStmtFunc (Located SynFunc)
             | SynStmtProc (Located SynProc)

instance Show SynStmt where
    show (SynStmtDef  x) = show x ++ "\n"
    show (SynStmtAttr x) = show x ++ "\n"
    show (SynStmtIf x) = show x ++ "\n"
    show (SynStmtWhile x) = show x ++ "\n"
    show (SynStmtCall x) = show x ++ "\n"
    show (SynStmtDefAttr x) = show x ++ "\n"
    show (SynStmtFor x) = show x ++ "\n"
    show (SynStmtFunc x) = show x ++ "\n"
    show (SynStmtProc x) = show x ++ "\n"

synstmt :: SynParser SynStmt
synstmt = locate $ try (fmap SynStmtDef syndef) 
               <|> fmap SynStmtDefAttr syndefattr
               <|> try (fmap SynStmtCall synpcall)
               <|> try (fmap SynStmtAttr synopattr)
               <|> fmap SynStmtAttr synattr
               <|> fmap SynStmtIf synifstr 
               <|> fmap SynStmtWhile synwhile
               <|> try (fmap SynStmtFor synfor)
               <|> fmap SynStmtFor synforp
               <|> fmap SynStmtFunc synfunc
               <|> fmap SynStmtProc synproc


-- | Syntactic construct for block
data SynBlock = SynBlock { getStmts :: [Located SynStmt] } deriving (Show)

-- | SynParser for block
synblock :: SynParser SynBlock
synblock = locate $
  do synlex LexLBraces
     stmts <- many synstmt
     synlex LexRBraces
     return $ SynBlock stmts

-- = Conditional structures:

-- == If / ifelse / else

-- | Syntactic construct for if/ifelse/else
data SynIf = SynIf [(Located SynExpr, Located SynBlock)]
                   (Maybe (Located SynBlock))
                   deriving (Show)

-- | SynParser for if/elseif/else structures
synifstr :: SynParser SynIf
synifstr = locate $
    do cblock1 <- synif
       cblocks <- many synelseif
       elseblock <- fmap Just synelse <|> return Nothing
       return $ SynIf (cblock1:cblocks) elseblock


-- | SynParser for if
synif :: SynSpecParser (Located SynExpr, Located SynBlock)
synif = do synlex LexIf
           expr <- synexpr
           content <- synblock
           return (expr, content)


-- | SynParser for else if
synelseif :: SynSpecParser (Located SynExpr, Located SynBlock)
synelseif = do try $ synlex LexElse >> synlex LexIf
               expr <- synexpr
               content <- synblock
               return (expr, content)

-- | SynParser for 'else'
synelse :: SynParser SynBlock
synelse = do synlex LexElse
             content <- synblock
             return content


-- == while

-- | Syntactic construct for 'while'
data SynWhile = SynWhile { getWhileCondition :: (Located SynExpr)
                         , getWhileBlock :: (Located SynBlock)
                         } deriving (Show)

-- | SynParser for while
synwhile :: SynParser SynWhile
synwhile = locate $
  do synlex LexWhile
     expr <- synexpr
     content <- synblock
     return $ SynWhile expr content

-- == for
-- | Syntactic construct for 'for'
data SynFor = SynForNP (Located SynIdent) (Located SynExpr) (Located SynBlock)
            | SynForP [Located SynIdent] [Located SynExpr] (Located SynBlock) deriving (Show)

-- | SynParser for 'for'
synfor :: SynParser SynFor
synfor = try synforp <|> synfornp

-- | SynParser of natural for
synfornp :: SynParser SynFor
synfornp = locate $
  do synlex LexFor
     i <- synident
     synlex LexIn
     range <- synexpr
     content <- synblock
     return $ SynForNP i range content

-- | SynParser for parallel for
synforp :: SynParser SynFor
synforp = locate $
  do synlex LexFor
     i <- fmap getidentlist synidentlist
     synlex LexParallel
     expr <- fmap getexprlist synexprlist
     content <- synblock
     return $ SynForP i expr content

-- | Syntactic construct for 'proc'
data SynProc = SynProc { getProcIdent :: (Located SynIdent)
                       , getProcTemplateType :: (Maybe (Located SynIdentList))
                       , getProcArgs ::  [SynTypedIdent]
                       , getProcBlock :: (Located SynBlock)
                       } deriving (Show)


instance Typed SynProc where
    toType p = ProcType $ map toAnnType $ getProcArgs p

instance Named SynProc where
    getName p = getName $ getProcIdent p

-- | SynParser for definition of a procedure
synproc :: SynParser SynProc
synproc = locate $
  do synlex LexProc
     ttype <- fmap Just syntident <|> return Nothing
     name <- synident
     synlex LexLParen
     i <- fmap gettypedidentlist synTypedIdentList
     synlex LexRParen
     content <- synblock
     return $ SynProc name ttype i content

getProcName :: SynProc -> String
getProcName p = getlabel . ignorepos . getProcIdent $ p

-- | Syntactic construct for 'func'
data SynFunc = SynFunc { getFuncIdent :: (Located SynIdent) 
                       , getTemplateType :: (Maybe (Located SynIdentList))
                       , getFuncArgs :: [SynTypedIdent]  
                       , getFuncRet :: [SynTypedIdent]
                       , getFuncBlock :: (Located SynBlock)
                       } deriving (Show)

instance Typed SynFunc where
    toType f = let rettypes = (map toType $ getFuncRet f)
                   argtypes = (map toAnnType $ getFuncArgs f) 
                in FuncType rettypes argtypes 

instance Named SynFunc where
    getName = getlabel . ignorepos . getFuncIdent

getFuncName :: SynFunc -> String
getFuncName f = getlabel . ignorepos . getFuncIdent $ f

-- | SynParser for definition of a function
synfunc :: SynParser SynFunc
synfunc = locate $
  do synlex LexFunc
     ttype <- fmap Just syntident <|> return Nothing
     argsreturn <- fmap gettypedidentlist synTypedIdentList 
     synlex LexAttr
     name <- synident
     synlex LexLParen
     argsparam <- fmap gettypedidentlist synTypedIdentList
     synlex LexRParen
     content <- synblock
     return $ SynFunc name ttype argsparam argsreturn content


-- | Syntactic construct for expression list
data SynExprList = SynExprList { getexprlist :: [Located SynExpr] }

instance Show SynExprList where
    show = show . getexprlist

-- | SynParser for list of expressions (not located)
synexprlist :: SynSpecParser SynExprList
synexprlist = fmap SynExprList $ sepBy synexpr (synlex LexComma) 

-- | Syntactic construct for function/procedure call
data SynCall = SynCall { getFuncId :: Located SynIdent
                       , getTypeParam :: (Maybe (Located SynIdentList))
                       , getArgList :: SynExprList
                       }

instance Show SynCall where
    show (SynCall id Nothing list) = show id ++ "(" ++ show list ++ ")"
    show (SynCall id (Just param) list) = show id ++ "<" ++ show param ++ "<(" ++ show list ++ ")"

-- | SynParser for function call
syncall :: SynParser SynCall
syncall = locate $
    do fid <- synident
       template <- fmap Just syntident <|> return Nothing
       synlex LexLParen
       exprs <- synexprlist
       synlex LexRParen
       return $ SynCall fid template exprs


-- | SynParser for procedure call
synpcall :: SynParser SynCall
synpcall = 
    do call <- syncall
       synlex LexSemicolon
       return call


-- | Syntactic construct for expressions
data SynExpr = SynIdentExpr (Located SynIdent)
             | SynLitIntExpr (Located SynLitInt)
             | SynLitFloatExpr (Located SynLitFloat)
             | SynLitBoolExpr (Located SynLitBool)
             | SynCallExpr (Located SynCall)
             | SynArrow    LocSynExpr (Located SynIdent)
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
             | SynMat      [[LocSynExpr]]
             | SynIndex    LocSynExpr [LocSynExpr]


paren :: (Show a) => a -> String
paren x = "(" ++ show x ++ ")"

parenBin :: (Show a, Show b) => String -> a -> b -> String
parenBin op x y = "(" ++ show x ++ op ++ show y ++ ")"

parenUn :: (Show a) => String -> a -> String
parenUn op x = "(" ++ op ++ show x ++ ")"

instance Show SynExpr where
    show (SynPar x)        = paren x
    show (SynArrow x y)    = parenBin "->" x y
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
    show (SynIdentExpr id) = show id
    show (SynLitIntExpr lit) = show lit
    show (SynLitFloatExpr lit) = show lit
    show (SynLitBoolExpr lit) = show lit
    show (SynCallExpr call) = show call
    show (SynMat mat) = show mat
    show (SynIndex x ys) = show x ++ "[" ++ sepys ++ "]"
        where sepys = intercalate "," $ map show ys

type LocSynExpr = Located SynExpr

-- | Identifier expression syntactic parser
synexprIdent :: SynParser SynExpr
synexprIdent = locate $ fmap SynIdentExpr synident

-- | Integer literal expression syntactic parser
synexprLitInt :: SynParser SynExpr
synexprLitInt = locate $ fmap SynLitIntExpr synlitint

-- | Float literal expression syntactic parser
synexprLitFloat :: SynParser SynExpr
synexprLitFloat = locate $ fmap SynLitFloatExpr synlitfloat

-- | Bool literal expression syntactic parser
synexprLitBool :: SynParser SynExpr
synexprLitBool = locate $ fmap SynLitBoolExpr synlitbool

-- | Parenthesized expression syntactic parser
synexprPar :: SynParser SynExpr
synexprPar = locate $
    do synlex LexLParen
       expr <- synexpr
       synlex LexRParen
       return $ SynPar expr

-- | Function call expression syntactic parser
synexprCall :: SynParser SynExpr
synexprCall = locate $ fmap SynCallExpr syncall


-- | Matrix/vector expression syntactic parser
synexprMat :: SynParser SynExpr
synexprMat =
    let matrow = many synexpr
        matrows = matrow `sepBy` synlex LexSemicolon
     in locate $
         do synlex LexLBracket
            mat <- fmap SynMat matrows
            synlex LexRBracket
            return mat


-- | High precedence expression unit parser
synexprUnit :: SynParser SynExpr
synexprUnit = synexprPar <|> synexprLitInt
                         <|> synexprLitFloat
                         <|> synexprLitBool
                         <|> try synexprCall
                         <|> synexprMat
                         <|> synexprIdent


-- | Unit followes by arrow or index access parser
synexprAccess :: SynParser SynExpr
synexprAccess = 
    let arrowparser =
            do arrow <- synlex LexArrow
               ident <- synident
               return (getpos arrow, Left ident)
        indexparser =
            do lb <- synlex LexLBracket
               exprs <- synexpr `sepBy` synlex LexComma
               synlex LexRBracket
               return (getpos lb, Right exprs)
        buildexpr locexpr (loc, Left locident) =
            mklocated loc (SynArrow locexpr locident)
        buildexpr locexpr (loc, Right locidxs) =
            mklocated loc (SynIndex locexpr locidxs)
     in do expr <- synexprUnit
           accs <- many (arrowparser <|> indexparser)
           return $ foldl buildexpr expr accs




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
synexpr = buildExpressionParser synoptable synexprAccess

-- | Syntactic constructs 
-- | and SynParser for module
data SynModStmt = SynModStruct (Located SynStruct)
                | SynModDef (Located SynDef)
                | SynModProc (Located SynProc)
                | SynModFunc (Located SynFunc) deriving (Show)

data SynModule = SynModule 
                  { modName :: Located SynIdent
                  , modStmts :: [SynModStmt]
                  } deriving (Show)

synModStmt :: SynSpecParser SynModStmt
synModStmt = (fmap SynModStruct synstruct) <|> 
              (fmap SynModDef syndef) <|> 
              (fmap SynModProc synproc) <|> 
              (fmap SynModFunc synfunc)

-- | SynParser for whole module
synmodule :: SynParser SynModule
synmodule = locate $
    do synlex LexModule
       moduleName <- synident
       synlex LexSemicolon
       stmts <- many synModStmt
       eof
       return (SynModule moduleName stmts)
