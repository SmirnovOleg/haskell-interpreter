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
    {-extract $ parseMaybe defParser input where
        extract :: Maybe Expr -> IO ()
        extract (Nothing) = putStrLn (show ParseError) >> runRepl env
        extract (Just parsed) = -}
    process $ eval env (read input) where
        process :: (Env, Safe Expr) -> IO ()
        process (nenv, Right result) = putStrLn (show result) >> runRepl nenv
        process (nenv, Left error) = putStrLn (show error) >> runRepl env

        
fact :: Expr
fact = Def ("factorial") [NamePattern "n"] (IfThenElse (AppBinOp Eq (Ident "n") (IntLiteral 0)) (IntLiteral 1) (AppBinOp Mul (Ident "n") (App (Ident "factorial") (AppBinOp Sub (Ident "n") (IntLiteral 1)))))