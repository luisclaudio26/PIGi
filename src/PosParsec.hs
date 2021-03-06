module PosParsec where

import Text.Parsec.Prim 
import Text.Parsec.Pos
import Types

newtype Located a = Located (SourcePos, a) 

instance (Show a) => Show (Located a) where
    show loc = show $ ignorepos loc

instance (Typed a) => Typed (Located a) where
    toType loc = toType $ ignorepos loc

instance (AnnotatedTyped a) => AnnotatedTyped (Located a) where
    toAnnType loc = toAnnType $ ignorepos loc

instance (Named a) => Named (Located a) where
    getName loc = getName $ ignorepos loc

instance Functor Located where
    fmap f (Located (pos, x)) = Located (pos, f x)


-- | Remove position from located
ignorepos :: Located a -> a
ignorepos (Located p) = snd p

-- | Get position from located
getpos :: Located a -> SourcePos
getpos (Located p) = fst p

-- | Creates a located 
mklocated :: SourcePos -> a -> Located a
mklocated pos t = Located (pos, t)

-- | Adds position to parser
locate :: (Monad m) => ParsecT s u m a -> ParsecT s u m (Located a)
locate parser = do pos <- getPosition
                   t   <- parser
                   return $ mklocated pos t

-- | Removes postion from a parser
unlocate :: (Monad m) => ParsecT s u m (Located a) -> ParsecT s u m a
unlocate parser = fmap ignorepos parser

