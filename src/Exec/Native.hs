module Exec.Native (nativeProcs, nativeFuncs) where

import Prelude hiding (print)
import Types
import Exec.Prim


-- = Native procedures

printStr :: String -> Exec ()
printStr s = mkExec $ \state ->
    do putStr s
       return state 


printStrLn :: String -> Exec ()
printStrLn = printStr . (++"\n")


print :: [Val] -> Exec ()
print [(IntVal i)] = printStr $ show i
print [(FloatVal f)] = printStr $ show f
print [(BoolVal b)] = printStr $ show b
print [(MatVal _ _ mat)] = printStr $ show mat


printLn :: [Val] -> Exec ()
printLn v = do print v
               printStr "\n"


nativeProcs :: [Proc]
nativeProcs =
    [NativeProc "print" (ProcType [IntType]) print
    ,NativeProc "print" (ProcType [FloatType]) print
    ,NativeProc "print" (ProcType [BoolType]) print
    ,NativeProc "print" (ProcType [MatType]) print
    ,NativeProc "println" (ProcType [IntType]) printLn
    ,NativeProc "println" (ProcType [FloatType]) printLn
    ,NativeProc "println" (ProcType [BoolType]) printLn
    ,NativeProc "println" (ProcType [MatType]) printLn
    ]

-- = Native functions

rows :: [Val] -> Exec [Val]
rows [MatVal r _ _] = return $ [IntVal r]


cols :: [Val] -> Exec [Val]
cols [MatVal _ c _] = return $ [IntVal c]


readLine :: Exec String
readLine = mkEval $ const getLine


readInt :: [Val] -> Exec [Val]
readInt [] =
    do ln <- readLine
       return [IntVal $ read ln]


readFloat :: [Val] -> Exec [Val]
readFloat [] =
    do ln <- readLine
       return [FloatVal $ read ln]


roundFloat :: [Val] -> Exec [Val]
roundFloat [FloatVal f] = return [IntVal $ round f]


floorFloat :: [Val] -> Exec [Val]
floorFloat [FloatVal f] = return [IntVal $ floor f]


ceilFloat :: [Val] -> Exec [Val]
ceilFloat [FloatVal f] = return [IntVal $ ceiling f]


floatCast :: [Val] -> Exec [Val]
floatCast [IntVal i] = return [FloatVal $ fromIntegral i]


nativeFuncs :: [Func]
nativeFuncs =
    [NativeFunc "rows" (FuncType [IntType] [MatType]) rows
    ,NativeFunc "cols" (FuncType [IntType] [MatType]) cols
    ,NativeFunc "readint" (FuncType [] [IntType]) readInt
    ,NativeFunc "readfloat" (FuncType [] [FloatType]) readFloat
    ,NativeFunc "round" (FuncType [IntType] [FloatType]) roundFloat
    ,NativeFunc "floor" (FuncType [IntType] [FloatType]) floorFloat
    ,NativeFunc "ceil" (FuncType [IntType] [FloatType]) ceilFloat
    ,NativeFunc "float" (FuncType [FloatType] [IntType]) floatCast
    ]


