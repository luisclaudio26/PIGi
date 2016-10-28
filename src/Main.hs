module Main(main) where

import System.Environment
import Text.Parsec (parse)
import PosParsec
import Lexical
import Syntactic

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
    mapM_ (print . snd) tokens

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
    error "Interpreter not implemented yet"
run [] = putStrLn "Usage [(-l | -s)] filename"

main :: IO ()
main = do putStrLn "<< PIG language interpreter >>"
          getArgs >>= run 
