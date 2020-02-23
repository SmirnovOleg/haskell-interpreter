module REPL where

import AST
import Parser (replParser)
import Interpreter
import System.IO
import Text.Megaparsec
import qualified Data.Map as Map
import qualified Data.List as List
import Runtime (tvs)
import Control.Monad.Trans.Except
import Control.Monad.State


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


runRepl :: Env -> IO ()
runRepl env = do
    input <- prompt "MiniHaskell> "
    case input of
        ":q" -> return ()
        ':':'t':' ':expr ->
            case runParser replParser "" expr of
                Left parseError -> (putStrLn (errorBundlePretty parseError) >> runRepl env)
                Right expr'     -> process $ getType env expr' where
                    process (Right tp) = 
                        case (expr', tp) of 
                            (App _ _, HVar _) -> putStrLn (expr ++ " :: " ++ show tp) >> runRepl env
                            (_, HVar _)       -> putStrLn ("NotInScope " ++ show expr) >> runRepl env
                            otherwise         -> putStrLn (expr ++ " :: " ++ show tp) >> runRepl env
                    process (Left error) = putStrLn (show error) >> runRepl env
        otherwise ->
            case runParser replParser "" input of
                Left parseError        -> (putStrLn (errorBundlePretty parseError) >> runRepl env)
                Right expr@(Def _ _ _) -> process $ runState (runExceptT $ eval expr) env
                Right expr@(Where _ _) -> process $ runState (runExceptT $ eval expr) env
                Right expr@(Let _)     -> process $ runState (runExceptT $ eval expr) env
                Right expr             -> typeCheckAndProcess expr $ getType env expr 
            where
                typeCheckAndProcess expr (Left error) = putStrLn (show error) >> runRepl env
                typeCheckAndProcess expr (Right tp) = process $ runState (runExceptT $ eval expr) env
                process (Right None, nenv) = runRepl nenv
                process (Right result, nenv) = putStrLn (prettyPrint result) >> runRepl nenv
                process (Left error, _) = putStrLn (show error) >> runRepl env                 
                        