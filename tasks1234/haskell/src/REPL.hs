module REPL where

import AST
import Parser (replParser)
import Interpreter
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map
import Runtime (runtime)


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

runRepl :: Env -> IO ()
runRepl env = do
    input <- prompt "MiniHaskell> "
    if input == ":q" then return () else
        case runParser replParser "" input of
            (Left parseError) -> (putStrLn (errorBundlePretty parseError) >> runRepl env)
            (Right expr) -> process $ eval env expr where
                process :: (Env, Safe Expr) -> IO ()
                process (nenv, Right None) = runRepl nenv
                process (nenv, Right result) = putStrLn (prettyPrint result) >> runRepl nenv
                process (nenv, Left error) = putStrLn (show error) >> runRepl env