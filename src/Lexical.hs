module Lexical where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Number as Number
import PosParsec

-- | Lexical token type
data LexToken = LexLParen         -- ^ @(@ token
              | LexRParen         -- ^ @)@ token
              | LexLBracket       -- ^ @[@ token
              | LexRBracket       -- ^ @]@ token
              | LexLBraces        -- ^ @{@ token
              | LexRBraces        -- ^ @}@ token
              | LexColon          -- ^ @:@ token
              | LexSemicolon      -- ^ @;@ token
              | LexComma          -- ^ @,@ token
              | LexArrow          -- ^ @->@ token
              | LexParallel       -- ^ @//@ token
              | LexRange          -- ^ @..@ token
              | LexAttr           -- ^ @=@ token
              | LexPlusAttr       -- ^ @+=@ token
              | LexMinusAttr      -- ^ @-=@ token
              | LexTimesAttr      -- ^ @*=@ token
              | LexDivAttr        -- ^ @/=@ token
              | LexPlus           -- ^ @+@ token
              | LexMinus          -- ^ @-@ token
              | LexTimes          -- ^ @*@ token
              | LexDiv            -- ^ @/@ token
              | LexDot            -- ^ @.@ token
              | LexBitAnd         -- ^ @&@ token
              | LexBitOr          -- ^ @|@ token
              | LexBitNot         -- ^ @!@ token
              | LexBitXor         -- ^ @~@ token
              | LexLShift         -- ^ @<<@ token
              | LexRShift         -- ^ @>>@ token
              | LexEQ             -- ^ @==@ token
              | LexNEQ            -- ^ @=/=@ token
              | LexLT             -- ^ @<@ token
              | LexLE             -- ^ @<=@ token
              | LexGT             -- ^ @>@ token
              | LexGE             -- ^ @>=@ token
              | LexAnd            -- ^ @and@ token
              | LexOr             -- ^ @or@ token
              | LexNot            -- ^ @not@ token
              | LexXor            -- ^ @xor@ token
              | LexDef            -- ^ @def@ token
              | LexFunc           -- ^ @func@ token
              | LexStruct         -- ^ @struct@ token
              | LexIf             -- ^ @if@ token
              | LexElse           -- ^ @else@ token
              | LexIn             -- ^ @in@ token
              | LexIdent String   -- ^ identifier
              | LexLitInt Int     -- ^ @int@ literal
              | LexLitFloat Float -- ^ @float@ literal
              deriving (Show, Eq)

-- | Map between keywords and lexical tokens
keywordTable :: [(String, LexToken)]
keywordTable = [("and", LexAnd)
               ,("or", LexOr)
               ,("not", LexNot)
               ,("xor", LexXor)
               ,("def", LexDef)
               ,("func", LexFunc)
               ,("struct", LexStruct)
               ,("if", LexIf)
               ,("else", LexElse)
               ,("in", LexIn)
               ]

-- | Map between symbols and lexical tokens
symbolTable :: [(String, LexToken)]
symbolTable = [("(", LexLParen)
              ,(")", LexRParen)
              ,("[", LexLBracket)
              ,("]", LexRBracket)
              ,("{", LexLBraces)
              ,("}", LexRBraces)
              ,(":", LexColon)
              ,(";", LexSemicolon)
              ,(",", LexComma)
              ,("=/=", LexNEQ)
              ,("==", LexEQ)
              ,("=", LexAttr)
              ,("<=", LexLE)
              ,("<", LexLT)
              ,(">=", LexGE)
              ,(">", LexGT)
              ,("//", LexParallel)
              ,("+=", LexPlusAttr)
              ,("+", LexPlus)
              ,("->", LexArrow)
              ,("-=", LexMinusAttr)
              ,("-", LexMinus)
              ,("*=", LexTimesAttr)
              ,("*", LexTimes)
              ,("/=", LexDivAttr)
              ,("/", LexDiv)
              ,("&", LexBitAnd)
              ,("|", LexBitOr)
              ,("~", LexBitNot)
              ,("..", LexRange)
              ,(".", LexDot)]

-- | Lists all keywords.
keywords :: [String]
keywords = map fst keywordTable

-- | A lexical token combined with its source code position
type PosLexToken = Located LexToken

-- | A parser for a lexical token.
type LexParser = Parsec String () PosLexToken

-- | Parses token by corresponding string.
lextoken :: LexToken -> String -> LexParser
lextoken token s = locate $
    do string s
       return token

-- | Parses identifier.
lexident :: LexParser
lexident = locate $
    do c  <- letter <|> char '_'
       cs <- many (alphaNum <|> char '_')
       let word = c:cs in
           if word `elem` keywords
              then fail "identifier"
              else return (LexIdent word)

-- | Parses a token based on the given table
lextable :: [(String, LexToken)] -> LexParser
lextable [] = fail "tabled"
lextable ((s, token):ts) = try(lextoken token s) <|> lextable ts

-- | Parses a reserved word
lexreserved :: LexParser
lexreserved = lextable keywordTable <|> lextable symbolTable

-- | Parses a floating number literal
lexfloat :: LexParser
lexfloat = locate $ do
    v <- Number.floating
    return (LexLitFloat v)

-- | Parses a integer literal
lexint :: LexParser
lexint = locate $ do
    v <- Number.int
    return (LexLitInt v)

-- | Parses a number
lexnumber :: LexParser
lexnumber = try(lexfloat) <|> lexint

-- | Parses any valid lexical token
lexunit :: LexParser
lexunit = do
    tk <- try(lexnumber) <|> try(lexident) <|> lexreserved
    spaces
    return tk

-- | PIG full lexical parser
lexparser :: Parsec String () [PosLexToken]
lexparser = do
    spaces
    tks <- many lexunit
    eof
    return tks
