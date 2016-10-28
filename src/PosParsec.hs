module PosParsec (Located, locate) where

import Text.Parsec.Prim 
import Text.Parsec.Pos

type Located a = (SourcePos, a)

-- | Adds position to parser
locate :: (Monad m) => ParsecT s u m a -> ParsecT s u m (Located a)
locate parser = do pos <- getPosition
                   t   <- parser
                   return (pos, t)

-- | Removes postion from a parser
unlocate :: (Monad m) => ParsecT s u m (Located a) -> ParsecT s u m a
unlocate parser = fmap snd parser
