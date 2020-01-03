module REPL where

import AST
import Parser (replParser)
import Interpreter
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map
import qualified Data.List as List


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


--getType :: Expr -> Env -> [Name] -> TypedEnv -> Safe Type
--getType expr env tvs tenv = do
--    (tvs', tenv') <- inferType expr tvs tenv
    


runRepl :: Env -> [Name] -> TypedEnv -> IO ()
runRepl env tvs tenv = do
    input <- prompt "MiniHaskell> "
    case input of
        ":q" -> return ()
        ':':'t':' ':expr ->
            case runParser replParser "" expr of
                (Left parseError) -> (putStrLn (errorBundlePretty parseError) >> runRepl env tvs tenv)
                (Right expr) -> 
                    case expr of
                        (Ident name) -> process $ getTypeByName name env tvs tenv
                        otherwise    -> process $ inferType expr tvs tenv
                    where
                        process (Right (tvs', tenv')) = 
                            putStrLn (show $ Map.lookup expr tenv') >> runRepl env tvs' tenv'
                        process (Left error) = 
                            putStrLn (show error) >> runRepl env tvs tenv
        otherwise ->
            case runParser replParser "" input of
            (Left parseError) -> (putStrLn (errorBundlePretty parseError) >> runRepl env tvs tenv)
            (Right expr) -> process $ eval env expr where
                process (nenv, Right None) = runRepl nenv tvs tenv
                process (nenv, Right result) = putStrLn (prettyPrint result) >> runRepl nenv tvs tenv
                process (nenv, Left error) = putStrLn (show error) >> runRepl env tvs tenv