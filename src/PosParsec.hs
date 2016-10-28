module PosParsec where

import Text.Parsec.Prim 
import Text.Parsec.Pos

type Located a = (SourcePos, a)

-- | Remove position from located
ignorepos :: Located a -> a
ignorepos = snd

-- | Get position from located
getpos :: Located a -> SourcePos
getpos = fst

-- | Creates a located 
mklocated :: SourcePos -> a -> Located a
mklocated pos t = (pos, t)

-- | Adds position to parser
locate :: (Monad m) => ParsecT s u m a -> ParsecT s u m (Located a)
locate parser = do pos <- getPosition
                   t   <- parser
                   return (pos, t)

-- | Removes postion from a parser
unlocate :: (Monad m) => ParsecT s u m (Located a) -> ParsecT s u m a
unlocate parser = fmap ignorepos parser

