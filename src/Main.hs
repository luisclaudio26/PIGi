module Main(main) where

import System.Environment
import Text.Parsec (parse)
import PosParsec
import Lexical
import Syntactic
import Interpreter

runinterpreter :: SynModule -> IO ()
runinterpreter mod = {-
    do let stmts = case mod of (SynModule x) -> x
           exec = runstmts (map ignorepos stmts)
       execIO exec State -}
       return ()


runsynparser :: [PosLexToken] -> String -> IO (Located SynModule)
runsynparser tokens filename =
    do let result = parse synmodule filename tokens
       case result of
            Left msg -> do print msg
                           fail "syntactic error"
            Right syntree -> return syntree


printsyn :: SynModule -> IO ()
printsyn = print


printlex :: [PosLexToken] -> IO ()
printlex tokens = 
    mapM_ (print . ignorepos) tokens


runlexparser :: String -> String -> IO [PosLexToken]
runlexparser filename input =
    let result = parse lexparser filename input
    in case result of
         Left msg -> do print msg
                        fail "lexical error"

         Right tokens -> return tokens


run :: [String] -> IO ()
run args =
    do file <- readFile filename
       lex  <- runlexparser filename file
       if elem "-l" opts
          then printlex lex
          else return ()
       syn <- fmap ignorepos (runsynparser lex filename)
       if elem "-s" opts
          then printsyn syn
          else return ()
       if length opts == 0
          then runinterpreter syn
          else return ()

    where isopt (c:cs) = c == '-'
          opts = filter isopt args
          filename = head (filter (not . isopt) args)


main :: IO ()
main = do putStrLn "<< PIG language interpreter >>"
          getArgs >>= run 
