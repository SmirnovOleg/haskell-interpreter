module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List


eval :: Env -> Expr -> Safe Expr

eval _ int@(IntLiteral _) = return int
eval _ bool@(BoolLiteral _) = return bool
eval _ char@(CharLiteral _) = return char

eval env (Ident name) = getByName env name

eval env (IfThenElse pred a b) = do
    result <- eval env pred
    if result == (BoolLiteral True) then 
        eval env a 
    else 
        eval env b

eval env (AppBinOp l op r) = do
    l <- eval env l
    r <- eval env r
    eval env (App (Ident $ show op) [l, r])

eval env (AppUnOp op x) = do
    x <- eval env x
    eval env (App (Ident $ show op) [x])

eval env (App (Ident func) args) = do
    lambda <- getByName env func
    result <- apply lambda args
    return result

-- eval env (Def (Name func) params body) = do
--     newEnv <- setByName env func (Lambda params body env)


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