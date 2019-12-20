module REPL where

import AST
import Parser
import Interpreter
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map

createBinOpLambda :: BinOp -> Safe Expr
createBinOpLambda op = Right $ Lambda 
                                [NamePattern "x", NamePattern "y"] 
                                (AppBinOp (Ident "x") op (Ident "y"))
                                Map.empty

createUnOpLambda :: UnOp -> Safe Expr
createUnOpLambda op = Right $ Lambda 
                                [NamePattern "x"] 
                                (AppUnOp op (Ident "x"))
                                Map.empty

builtInFunctions :: [(Name, Safe Expr)]
builtInFunctions = [ ("+", createBinOpLambda Add)
                   , ("-", createBinOpLambda Sub)
                   , ("*", createBinOpLambda Mul)
                   , ("`div`", createBinOpLambda Div)
                   , ("&&", createBinOpLambda And)
                   , ("||", createBinOpLambda Or)
                   , ("==", createBinOpLambda Eq)
                   , (">", createBinOpLambda Gt)
                   , ("<", createBinOpLambda Ls)
                   , ("++", createBinOpLambda Concat)
                   , (":", createBinOpLambda Push)
                   , ("neg", createUnOpLambda Neg)
                   , ("not", createUnOpLambda Not)
                   , ("fst", createUnOpLambda Fst)
                   , ("snd", createUnOpLambda Snd)
                   , ("head", createUnOpLambda Head)
                   , ("tail", createUnOpLambda Tail)]

runtime :: Env
runtime = Map.fromList builtInFunctions

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