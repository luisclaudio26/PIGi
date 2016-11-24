module Exec.Expr where

import Data.Bits 
import Data.Foldable
import Control.Monad
import Exec.Prim
import PosParsec
import Syntactic

-- = Expressions

-- | Execution to exponentiation
expVal :: Val -> Val -> Exec Val
expVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 ** f2


-- | Execution to negate a value
negVal :: Val -> Exec Val
negVal (IntVal i) = return $ IntVal $ -i
negVal (FloatVal f) = return $ FloatVal $ -f
negVal (MatVal _ _ mat) = matUnOp negVal mat


-- | Execution to invert all bits
bitNotVal :: Val -> Exec Val
bitNotVal (IntVal i) = return $ IntVal $ complement i


-- | Execution to multiply two values
timesVal :: Val -> Val -> Exec Val
timesVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 * i2
timesVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 * f2
timesVal (MatVal m1 n1 mat1) (MatVal m2 n2 mat2)
    | n1 == m2 = fmap (MatVal m1 n2) $ mapM (mapM valAt) indxs
    | otherwise = error "invaid matrix multiplication dimensions"
    where
        -- matrix multiplication 
        indxs = [[(i, j) | j <- [0..(n2-1)]] | i <- [0..(m1-1)]]
        sumVals = foldlM plusVal (FloatVal 0)
        mults i j = zipWithM timesVal (matRow mat1 i) (matCol mat2 j)
        valAt (i,j) = mults i j >>= sumVals


-- | Execution to multiply two values
divVal :: Val -> Val -> Exec Val
divVal (IntVal i1) (IntVal 0) = error "Division by 0"
divVal (IntVal i1) (IntVal i2) = return $ IntVal $ div i1 i2
divVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 / f2


-- | Execution to multiply two matrices elementwise
dotTimesVal :: Val -> Val -> Exec Val
dotTimesVal (MatVal _ _ mat1) (MatVal _ _ mat2) = matBinOp timesVal mat1 mat2


-- | Execution to divide two matrices elementwise
dotDivVal :: Val -> Val -> Exec Val
dotDivVal (MatVal _ _ mat1) (MatVal _ _ mat2) = matBinOp divVal mat1 mat2


-- | Execution to multiply two values
modVal :: Val -> Val -> Exec Val
modVal (IntVal i1) (IntVal 0) = error "Division by 0"
modVal (IntVal i1) (IntVal i2) = return $ IntVal $ mod i1 i2
modVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 - f2 * (fromInteger . floor $ f1 / f2)


-- | Execution to add two values
plusVal :: Val -> Val -> Exec Val
plusVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 + i2
plusVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 + f2
plusVal (MatVal _ _ mat1) (MatVal _ _ mat2) = matBinOp plusVal mat1 mat2


-- | Execution to subtract two values
minusVal :: Val -> Val -> Exec Val
minusVal (IntVal i1) (IntVal i2) = return $ IntVal $ i1 - i2
minusVal (FloatVal f1) (FloatVal f2) = return $ FloatVal $ f1 - f2
minusVal (MatVal _ _ mat1) (MatVal _ _ mat2) = matBinOp minusVal mat1 mat2


-- | Execution for @>>@
rshiftVal :: Val -> Val -> Exec Val
rshiftVal (IntVal i1) (IntVal i2) = return $ IntVal $ shiftR i1 i2


-- | Execution for @<<@
lshiftVal :: Val -> Val -> Exec Val
lshiftVal (IntVal i1) (IntVal i2) = return $ IntVal $ shiftL i1 i2


-- | Execution for @==@
eqVal :: Val -> Val -> Exec Val
eqVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 == i2
eqVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 == f2


-- | Execution for @=/=@
neqVal :: Val -> Val -> Exec Val
neqVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 /= i2
neqVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 /= f2


-- | Execution for @<@
ltVal :: Val -> Val -> Exec Val
ltVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 < i2
ltVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 < f2


-- | Execution for @<=@
leVal :: Val -> Val -> Exec Val
leVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 <= i2
leVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 <= f2


-- | Execution for @>@
gtVal :: Val -> Val -> Exec Val
gtVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 > i2
gtVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 > f2


-- | Execution for @>=@
geVal :: Val -> Val -> Exec Val
geVal (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 >= i2
geVal (FloatVal f1) (FloatVal f2) = return $ BoolVal $ f1 >= f2


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


-- | Build a matrix from list of lists
mkMat :: [[Val]] -> Val
mkMat p = MatVal (length p) (length $ head p) p


-- | Extract row from matrix
matRow :: [[Val]] -> Int -> [Val]
matRow mat i = (mat !! i) 


-- | Extract column from matrix
matCol :: [[Val]] -> Int -> [Val]
matCol mat j = map (!!j) mat


-- | Build matrix binary operation execution from scalar operation
matUnOp :: (Val -> Exec Val) -- ^ scalar operation
        -> [[Val]] -- ^ matrix
        -> Exec Val
matUnOp op mat = fmap mkMat $ mapM (mapM op) mat


-- | Build matrix binary operation execution from scalar operation
matBinOp :: (Val -> Val -> Exec Val) -- ^ scalar operation
         -> [[Val]] -- ^ first matrix
         -> [[Val]] -- ^ second matrix
         -> Exec Val
matBinOp op mat1 mat2 = fmap mkMat $ zipWithM (zipWithM op) mat1 mat2

