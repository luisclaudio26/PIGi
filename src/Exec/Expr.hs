module Exec.Expr where

import Exec.Prim
import PosParsec
import Syntactic

-- = Expressions

-- | Execution to add two values
addVal :: Val -> Val -> Exec Val
addVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 + i2
addVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 + f2
addVal _ _ = return None


-- | Execution to multiply two values
timesVal :: Val -> Val -> Exec Val
timesVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 * i2
timesVal _ _ = return None


-- | Execution to 'not' a value
notVal :: Val -> Exec Val
notVal (BoolVal b) = return $ BoolVal $ not b
notVal _ = return None


-- | Execution to 'and' two values
andVal :: Val -> Val -> Exec Val
andVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 && b2
andVal _ _ = return None


-- | Execution to 'or' two values
orVal :: Val -> Val -> Exec Val
orVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 || b2
orVal _ _ = return None


-- | Execution to 'xor' two values
xorVal :: Val -> Val -> Exec Val
xorVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ (b1 && not b2) || (not b1 && b2)
xorVal _ _ = return None


-- | Execution to run binary operation
evalUn :: (Val -> Exec Val) -- ^ Value computation function
       -> (Located SynExpr) -- ^ Expression
       -> Exec Val
evalUn f e =
    do v <- evalExpr e
       f v


-- | Execution to run binary operation
evalBin :: (Val -> Val -> Exec Val) -- ^ Value computation function
        -> (Located SynExpr) -- ^ Left expression
        -> (Located SynExpr) -- ^ Right expression
        -> Exec Val
evalBin f e1 e2 =
    do v1 <- evalExpr e1
       v2 <- evalExpr e2
       f v1 v2


-- | Execution to evaluate expression
evalExpr :: (Located SynExpr) -> Exec Val
evalExpr = eval . ignorepos
    where eval :: SynExpr -> Exec Val

          eval (SynLitIntExpr locint) =
              return $ IntVal $ getint . ignorepos $ locint

          eval (SynLitBoolExpr locbool) =
              return $ BoolVal $ getbool . ignorepos $ locbool

          eval (SynIdentExpr locident) =
              do var <- findVar $ ignorepos locident
                 return $ getVarValue var

          eval (SynPar e) = evalExpr e

          eval (SynPlus e1 e2) = evalBin addVal e1 e2

          eval (SynTimes e1 e2) = evalBin timesVal e1 e2

          eval (SynNot e) = evalUn notVal e

          eval (SynAnd e1 e2) = evalBin andVal e1 e2

          eval (SynOr e1 e2) = evalBin orVal e1 e2

          eval (SynXor e1 e2) = evalBin xorVal e1 e2


