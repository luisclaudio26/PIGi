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


