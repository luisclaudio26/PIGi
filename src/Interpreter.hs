module Interpreter where

import Control.Monad (ap)
import Syntactic

data ProgramState = State

data Exec a = Exec { execIO :: ProgramState -> IO ProgramState
                   , evalIO :: ProgramState -> IO a }

execmap :: (a -> b) -> Exec a -> Exec b
execmap f (Exec x v) = Exec x (fmap f . v) 

execunit :: a -> Exec a
execunit x = Exec return (const $ return x)

execbind :: Exec a -> (a -> Exec b) -> Exec b
execbind m k = Exec execS evalS
    where execS s1 = do val <- evalIO m s1
                        s2  <- execIO m s1
                        execIO (k val) s2

          evalS s1 = do val <- evalIO m s1
                        s2  <- execIO m s1
                        evalIO (k val) s2

instance Functor Exec where
    fmap = execmap

instance Applicative Exec where
    pure = execunit
    (<*>) = ap

instance Monad Exec where
    return = execunit
    (>>=) = execbind

runstmt :: SynStruct -> Exec ()
runstmt stmt = execunit ()

runstmts :: [SynStruct] -> Exec ()
runstmts stmts = mapM_ runstmt stmts
