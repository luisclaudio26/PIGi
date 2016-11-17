module Exec.Expr where

import Data.Bits 
import Exec.Prim
import PosParsec
import Syntactic

-- = Expressions

-- | Execution to negate a value
negVal :: Val -> Exec Val
negVal (IntVal i) = return $ IntVal $ i


-- | Execution to invert all bits
bitNotVal :: Val -> Exec Val
bitNotVal (IntVal i) = return $ IntVal $ complement i


-- | Execution to multiply two values
timesVal :: Val -> Val -> Exec Val
timesVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 * i2


-- | Execution to multiply two values
divVal :: Val -> Val -> Exec Val
divVal (IntVal i1) (IntVal 0) = error "Division by 0"
divVal (IntVal i1) (IntVal i2) = return $ IntVal $ div i1 i2


-- | Execution to multiply two values
modVal :: Val -> Val -> Exec Val
modVal (IntVal i1) (IntVal 0) = error "Division by 0"
modVal (IntVal i1) (IntVal i2) = return $ IntVal $ mod i1 i2


-- | Execution to add two values
plusVal :: Val -> Val -> Exec Val
plusVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 + i2
plusVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 + f2


-- | Execution to subtract two values
minusVal :: Val -> Val -> Exec Val
minusVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 - i2


-- | Execution for @>>@
rshiftVal :: Val -> Val -> Exec Val
rshiftVal (IntVal i1) (IntVal i2) = return $ IntVal $ shiftR i1 i2


-- | Execution for @<<@
lshiftVal :: Val -> Val -> Exec Val
lshiftVal (IntVal i1) (IntVal i2) = return $ IntVal $ shiftL i1 i2


-- | Execution for @==@
eqVal :: Val -> Val -> Exec Val
eqVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 == i2


-- | Execution for @=/=@
neqVal :: Val -> Val -> Exec Val
neqVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 /= i2


-- | Execution for @<@
ltVal :: Val -> Val -> Exec Val
ltVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 < i2


-- | Execution for @<=@
leVal :: Val -> Val -> Exec Val
leVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 <= i2


-- | Execution for @>@
gtVal :: Val -> Val -> Exec Val
gtVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 > i2


-- | Execution for @>=@
geVal :: Val -> Val -> Exec Val
geVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 >= i2


-- | Execution for bitwise and
bitAndVal :: Val -> Val -> Exec Val
bitAndVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 .&. i2


-- | Execution for bitwise xor
bitXorVal :: Val -> Val -> Exec Val
bitXorVal (IntVal i1) (IntVal i2) = return $ IntVal $ xor i1 i2


-- | Execution for bitwise or
bitOrVal :: Val -> Val -> Exec Val
bitOrVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 .|. i2


-- | Execution to 'not' a value
notVal :: Val -> Exec Val
notVal (BoolVal b) = return $ BoolVal $ not b


-- | Execution to 'and' two values
andVal :: Val -> Val -> Exec Val
andVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 && b2


-- | Execution to 'xor' two values
xorVal :: Val -> Val -> Exec Val
xorVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ (b1 && not b2) || (not b1 && b2)


-- | Execution to 'or' two values
orVal :: Val -> Val -> Exec Val
orVal (BoolVal b1) (BoolVal b2) = return $ BoolVal $ b1 || b2


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
          eval (SynNeg e) = evalUn negVal e
          eval (SynBitNot e) = evalUn bitNotVal e
          eval (SynTimes e1 e2) = evalBin timesVal e1 e2
          eval (SynDiv e1 e2) = evalBin divVal e1 e2
          eval (SynMod e1 e2) = evalBin modVal e1 e2
          eval (SynPlus e1 e2) = evalBin plusVal e1 e2
          eval (SynMinus e1 e2) = evalBin minusVal e1 e2
          eval (SynRShift e1 e2) = evalBin rshiftVal e1 e2
          eval (SynLShift e1 e2) = evalBin lshiftVal e1 e2
          eval (SynEQ e1 e2) = evalBin eqVal e1 e2
          eval (SynNEQ e1 e2) = evalBin neqVal e1 e2
          eval (SynLT e1 e2) = evalBin ltVal e1 e2
          eval (SynLE e1 e2) = evalBin leVal e1 e2
          eval (SynGT e1 e2) = evalBin gtVal e1 e2
          eval (SynGE e1 e2) = evalBin geVal e1 e2
          eval (SynBitAnd e1 e2) = evalBin bitAndVal e1 e2
          eval (SynBitXor e1 e2) = evalBin bitXorVal e1 e2
          eval (SynBitOr e1 e2) = evalBin bitOrVal e1 e2
          eval (SynNot e) = evalUn notVal e
          eval (SynAnd e1 e2) = evalBin andVal e1 e2
          eval (SynXor e1 e2) = evalBin xorVal e1 e2
          eval (SynOr e1 e2) = evalBin orVal e1 e2


