module Syntactic where

import Text.Parsec (eof)
import Text.Parsec.Prim
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

--| SynParser for definition of variable
syndef :: SynParser SynDef
syndef = locate $ 
  do synlex LexDef
     var <- synident
     synlex LexColon
     vartype <- synident  
     synlex LexSemicolon
     return (SynDef var vartype)

-- !! EVERYTHING BELOW THIS LINE IS WRONG !!

-- | Syntactic construct for module
data SynModule = SynModule [Located SynDef] deriving (Show)

-- | SynParser for whole module
synmodule :: SynParser SynModule
synmodule = locate $
    do ids <- many syndef
       eof
       return (SynModule ids)

