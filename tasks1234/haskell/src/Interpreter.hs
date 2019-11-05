module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List


instance Show Expr where
    show (IntLiteral x) = show x
    show (BoolLiteral x) = show x
    show lambda@(Lambda patterns body closure) = "<lambda>"


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
            calc _ Add _  = Left $ TypeError "Int in + operator expected"
            calc (IntLiteral a) Sub (IntLiteral b) = return $ IntLiteral (a - b)
            calc _ Sub _  = Left $ TypeError "Int in - operator expected"
            calc (IntLiteral a) Mul (IntLiteral b) = return $ IntLiteral (a * b)
            calc _ Mul _  = Left $ TypeError "Int in * operator expected"
            calc (IntLiteral a) Div (IntLiteral b) = return $ IntLiteral (a `div` b)
            calc _ Div _  = Left $ TypeError "Int in / operator expected"
            
            calc (BoolLiteral a) And (BoolLiteral b) = return $ BoolLiteral (a && b)
            calc _ And _  = Left $ TypeError "Bool in && operator expected"
            calc (BoolLiteral a) Or (BoolLiteral b) = return $ BoolLiteral (a || b)
            calc _ Or _  = Left $ TypeError "Bool in || operator expected"

            calc (IntLiteral a) Eq (IntLiteral b) = return $ BoolLiteral (a == b)
            calc (BoolLiteral a) Eq (BoolLiteral b) = return $ BoolLiteral (a == b)
            calc _ Eq _  = Left $ TypeError "Int or Bool in == operator expected"
            calc (IntLiteral a) Ls (IntLiteral b) = return $ BoolLiteral (a < b)
            calc _ Ls _  = Left $ TypeError "Int in < operator expected"
            calc (IntLiteral a) Gt (IntLiteral b) = return $ BoolLiteral (a > b)
            calc _ Gt _  = Left $ TypeError "Int in > operator expected"
            

eval env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ eval env x
        calc op x where
            calc Neg (IntLiteral a) = return $ IntLiteral (-a)
            calc Neg _  = Left $ TypeError "Int in unary - operator expected"

            calc Not (BoolLiteral a) = return $ BoolLiteral (not a)
            calc Not _  = Left $ TypeError "Bool in not operator expected"

eval env (App (Ident func) arg) = (env, result) where
    result = do
        lambda <- getByName env func
        newLambda <- apply env lambda arg
        snd $ eval env newLambda

eval env (Def func patterns body) = (nenv, return lambda) where
    nenv = setByName env func lambda
    lambda = Lambda patterns body nenv

eval env lambda@(Lambda patterns body closure) = (env, result) where 
    result = 
        if length patterns == 0 then
            snd $ eval closure body
        else
            return lambda


apply :: Env -> Expr -> Expr -> Safe Expr
apply env (Lambda patterns body closure) arg = 
    if length patterns == 0 then do
        newLambda <- snd $ eval closure body
        apply env newLambda arg
    else
        return $ (Lambda abstr body newClosure) where
            newClosure = bindNames closure (zip patterns [value])
            value = Lambda [] arg (Map.union closure env)
            abstr = tail patterns


getByName :: Env -> Name -> Safe Expr
getByName env name = case (Map.lookup name env) of
    (Nothing) -> Left (NotInScope name)
    (Just x)  -> x


setByName :: Env -> Name -> Expr -> Env
setByName env name expr = Map.insert name (return expr) env


bindNames :: Env -> [(Pattern, Expr)] -> Env
bindNames env [] = env
bindNames env ((NamePattern name, expr) : bindings) = Map.union other current where
    other = bindNames env bindings
    current = setByName env name expr