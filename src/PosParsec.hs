module PosParsec (locate) where

import Text.Parsec.Prim 
import Text.Parsec.Pos

-- | Adds position to parser
locate :: (Monad m) => ParsecT s u m a -> ParsecT s u m (SourcePos, a)
locate parser = do pos <- getPosition
                   t   <- parser
                   return (pos, t)

