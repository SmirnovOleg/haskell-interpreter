module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List


instance Show Expr where
    show (IntLiteral x) = show x
    show (BoolLiteral x) = show x
    show lambda@(Lambda params body closure) = "<Lambda>"


eval :: Env -> Expr -> (Env, Safe Expr)

eval env int@(IntLiteral _) = (env, return int)
eval env bool@(BoolLiteral _) = (env, return bool)
eval env char@(CharLiteral _) = (env, return char)

eval env (Ident name) = (env, result) where
    result = do
        lambda <- getByName env name
        snd $ eval env lambda

eval env (IfThenElse predicate a b) = (env, result) where
    result = do
        condition <- snd $ eval env predicate
        if condition == (BoolLiteral True) then 
            snd $ eval env a
        else
            snd $ eval env b

eval env (AppBinOp op l r) = (env, result) where
    result = do
        l <- snd $ eval env l
        r <- snd $ eval env r
        calc l op r where
            calc (IntLiteral a) Add (IntLiteral b) = return $ IntLiteral (a + b)
            calc _ Add _  = Left $ WrongType "IntLiteral in Add expected"

eval env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ eval env x
        calc op x where
            calc Neg (IntLiteral a) = return $ IntLiteral (-a)
            calc Neg _  = Left $ WrongType "IntLiteral in Neg expected"

eval env (App (Ident func) arg) = (env, result) where
    result = do
        lambda <- getByName env func
        newLambda <- apply lambda arg
        snd $ eval env newLambda

eval env (Def func params body) = (nenv, return lambda) where
    nenv = setByName env func lambda
    lambda = Lambda params body nenv

eval env lambda@(Lambda params body closure) = (env, result) where 
    result = 
        if length params == 0 then
            snd $ eval (Map.union closure env) body
        else
            return lambda


apply :: Expr -> Expr -> Safe Expr
apply (Lambda params body closure) arg = 
    if length params == 0 then do
        newLambda <- snd $ eval closure body
        apply newLambda arg
    else
        return $ (Lambda abstr body newClosure) where
            bindings = zip params [arg]
            newClosure = bindNames closure bindings
            abstr = tail params


getByName :: Env -> Name -> Safe Expr
getByName env name = case (Map.lookup name env) of
    (Nothing) -> Left (NotInScope name)
    (Just x)  -> x


setByName :: Env -> Name -> Expr -> Env
setByName env name expr = Map.insert name (return expr) env


bindNames :: Env -> [(Name, Expr)] -> Env
bindNames env [] = env
bindNames env ((name, expr):bindings) = Map.union other current where
    other = bindNames env bindings
    current = setByName env name expr