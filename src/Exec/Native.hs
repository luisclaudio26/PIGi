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


nativeFuncs :: [Func]
nativeFuncs =
    [NativeFunc "rows" (FuncType [IntType] [MatType]) rows
    ,NativeFunc "cols" (FuncType [IntType] [MatType]) cols
    ]
