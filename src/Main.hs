module Main(main) where

import System.Environment
import Text.Parsec (parse)
import PosParsec
import Lexical
import Syntactic
import Interpreter

runinterpreter :: String -> String -> IO ()
runinterpreter filename input =
    do mod <- runsynparser filename input
       let stmts = case (ignorepos mod) of (SynModule x) -> x
           exec = runstmts (map ignorepos stmts)
       execIO exec State
       return ()

runsynparser :: String -> String -> IO (Located SynModule)
runsynparser filename input =
    do tokens <- runlexparser filename input
       let result = parse synmodule filename tokens
       case result of
            Left msg -> do print msg
                           fail "syntactic error"
            Right syntree -> do print syntree
                                return syntree

printlextokens :: [PosLexToken] -> IO ()
printlextokens tokens = 
    mapM_ (print . ignorepos) tokens

runlexparser :: String -> String -> IO [PosLexToken]
runlexparser filename input =
    let result = parse lexparser filename input
    in case result of
         Left msg -> do print msg
                        fail "lexical error"

         Right tokens -> do printlextokens tokens
                            return tokens

run :: [String] -> IO ()
run ["-l", filename] =
    readFile filename >>= runlexparser filename >> return ()
run ["-l"] =
    getContents >>= runlexparser "(stdin)" >> return ()
run ["-s", filename] =
    readFile filename >>= runsynparser filename >> return ()
run [filename] =
    readFile filename >>= runinterpreter filename >> return ()
run [] = putStrLn "Usage [(-l | -s)] filename"

main :: IO ()
main = do putStrLn "<< PIG language interpreter >>"
          getArgs >>= run 
