module REPL where

import AST
import Parser
import Interpreter
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map


runtime :: Env
runtime = Map.empty


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


runRepl :: Env -> IO ()
runRepl env = do
    input <- prompt "MiniHaskell> "
    extract $ parseMaybe myParser input where

        extract :: Maybe Expr -> IO ()
        extract (Nothing) = putStrLn (show ParseError) >> runRepl env
        extract (Just parsed) = process $ eval env parsed --eval env (read input)

        process :: (Env, Safe Expr) -> IO ()
        process (nenv, Right result) = putStrLn (show result) >> runRepl nenv
        process (nenv, Left error) = putStrLn (show error) >> runRepl env