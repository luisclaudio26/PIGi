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


annInt = toAnnType IntType
annFloat = toAnnType FloatType
annBool = toAnnType BoolType
annMat = toAnnType MatType


nativeProcs :: [Proc]
nativeProcs =
    [NativeProc "print" (ProcType [annInt]) print
    ,NativeProc "print" (ProcType [annFloat]) print
    ,NativeProc "print" (ProcType [annBool]) print
    ,NativeProc "print" (ProcType [annMat]) print
    ,NativeProc "println" (ProcType [annInt]) printLn
    ,NativeProc "println" (ProcType [annFloat]) printLn
    ,NativeProc "println" (ProcType [annBool]) printLn
    ,NativeProc "println" (ProcType [annMat]) printLn
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
    [NativeFunc "rows" (FuncType [IntType] [annMat]) rows
    ,NativeFunc "cols" (FuncType [IntType] [annMat]) cols
    ,NativeFunc "readint" (FuncType [] [annInt]) readInt
    ,NativeFunc "readfloat" (FuncType [] [annFloat]) readFloat
    ,NativeFunc "round" (FuncType [IntType] [annFloat]) roundFloat
    ,NativeFunc "floor" (FuncType [IntType] [annFloat]) floorFloat
    ,NativeFunc "ceil" (FuncType [IntType] [annFloat]) ceilFloat
    ,NativeFunc "float" (FuncType [FloatType] [annInt]) floatCast
    ]


