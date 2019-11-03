module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List



eval :: Env -> Expr -> (Env, Safe Expr)

eval env int@(IntLiteral _) = (env, return int)
eval env bool@(BoolLiteral _) = (env, return bool)
eval env char@(CharLiteral _) = (env, return char)

eval env (Ident name) = (env, getByName env name)

eval env (IfThenElse predicate a b) = (env, result) where
    result = do
        condition <- snd $ eval env predicate
        if condition == (BoolLiteral True) then 
            snd $ eval env a
        else
            snd $ eval env b

eval env (AppBinOp l op r) = (env, result) where
    result = do
        l <- snd $ eval env l
        r <- snd $ eval env r
        snd $ eval env (App (Ident $ show op) [l, r])

eval env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ eval env x
        snd $ eval env (App (Ident $ show op) [x])

eval env (App (Ident func) args) = (env, result) where
    result = do
        lambda <- getByName env func
        app <- apply lambda args
        return app

eval env (Def func params body) = (nenv, return lambda) where
    nenv = setByName env func lambda
    lambda = Lambda params body nenv



apply :: Expr -> [Expr] -> Safe Expr
apply (Lambda params body closure) args = 
    (if length params < length args then
        (Left (WrongNumberOfArgs))
    else
        return $ (Lambda abstr body newClosure))
    where
        bindings = zip params args
        newClosure = bindNames closure bindings
        abstr = List.drop (length args) params


getByName :: Env -> Name -> Safe Expr
getByName env name = maybe  (Left (NotInScope name)) id (Map.lookup name env)

setByName :: Env -> Name -> Expr -> Env
setByName env name expr = Map.insert name (return expr) env

bindNames :: Env -> [(Name, Expr)] -> Env
bindNames env [] = env
bindNames env ((name, expr):bindings) = Map.union other current where
    other = bindNames env bindings
    current = setByName env name expr