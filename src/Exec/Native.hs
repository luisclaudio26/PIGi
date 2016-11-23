module Exec.Native (nativeProcs) where

import Prelude hiding (print)
import Types
import Exec.Prim

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

