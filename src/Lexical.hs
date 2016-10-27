module Lexical where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Number as Number
import PosParsec

-- | Lexical token type
data LexToken = LexLParen         -- (
              | LexRParen         -- )
              | LexLBracket       -- [
              | LexRBracket       -- ]
              | LexLBraces        -- {
              | LexRBraces        -- }
              | LexColon          -- :
              | LexSemicolon      -- ;
              | LexComma          -- ,
              | LexArrow          -- ->
              | LexParallel       -- //
              | LexRange          -- ..
              | LexAttr           -- =
              | LexPlusAttr       -- +=
              | LexMinusAttr      -- -=
              | LexTimesAttr      -- *=
              | LexDivAttr        -- /=
              | LexPlus           -- +
              | LexMinus          -- -
              | LexTimes          -- *
              | LexDiv            -- /
              | LexDot            -- .
              | LexBitAnd         -- &
              | LexBitOr          -- |
              | LexBitNot         -- !
              | LexBitXor         -- ~
              | LexLShift         -- <<
              | LexRShift         -- >>
              | LexEQ             -- ==
              | LexNEQ            -- =/=
              | LexLT             -- <
              | LexLE             -- <=
              | LexGT             -- >
              | LexGE             -- >=
              | LexAnd            -- and
              | LexOr             -- or
              | LexNot            -- not
              | LexXor            -- xor
              | LexDef            -- def
              | LexFunc           -- func
              | LexStruct         -- struct
              | LexIf             -- if
              | LexElse           -- else
              | LexIn             -- in
              | LexIdent String   -- identifier
              | LexLitInt Int     -- int literal
              | LexLitFloat Float -- float literal
              deriving (Show)

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
type PosToken = (SourcePos, LexToken)

-- | A parser for a lexical token.
type LexParser = Parsec String () PosToken

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
lextable ((s, token):ts) = lextoken token s <|> lextable ts

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
lexparser :: Parsec String () [PosToken]
lexparser = do
    spaces
    tks <- many lexunit
    eof
    return tks
