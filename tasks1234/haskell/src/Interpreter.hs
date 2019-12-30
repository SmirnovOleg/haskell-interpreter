module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List


eval :: Env -> Expr -> (Env, Safe Expr)
eval env int@(IntLiteral _) = (env, return int)
eval env bool@(BoolLiteral _) = (env, return bool)
eval env char@(CharLiteral _) = (env, return char)

eval env (Undefined) = (env, (Left UndefinedNameError))
eval env (None) = (env, return None)

eval env (ListExpr list) = (env, result) where
    result = ListExpr <$> mapM (\x -> snd $ eval env x) list

eval env (PairExpr (x,y)) = (env, result) where
    result = do
        p1 <- snd $ eval env x
        p2 <- snd $ eval env y
        return $ PairExpr (p1, p2)

eval env (IfThenElse predicate a b) = (env, result) where
    result = do
        condition <- snd $ eval env predicate
        if condition == (BoolLiteral True) then 
            snd $ eval env a
        else if condition == (BoolLiteral False) then
            snd $ eval env b
        else Left $ TypeError "Boolean condition in `if` expected"

eval env (AppBinOp l op r) = (env, result) where
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
            calc _ Div _  = Left $ TypeError "Int in `div` operator expected"
            
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
            
            calc (ListExpr xs) Concat (ListExpr ys) = return $ ListExpr (xs ++ ys)
            calc _ Concat _  = Left $ TypeError "Lists in `concat` operator expected"
            calc (x) Push (ListExpr xs) = return $ ListExpr (x : xs)
            calc _ Push _  = Left $ TypeError "Expr and [Expr] in : operator expected"

eval env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ eval env x
        calc op x where
            calc Neg (IntLiteral a) = return $ IntLiteral (-a)
            calc Neg _  = Left $ TypeError "Int in unary - operator expected"

            calc Not (BoolLiteral a) = return $ BoolLiteral (not a)
            calc Not _  = Left $ TypeError "Bool in not operator expected"

            calc Fst (PairExpr (p1, _)) = return $ p1
            calc Fst _  = Left $ TypeError "Pair in `fst` function expected"
            calc Snd (PairExpr (_, p2)) = return $ p2
            calc Snd _  = Left $ TypeError "Pair in `snd` function expected"
            
            calc Head (ListExpr (head:tail)) = return $ head
            calc Head _  = Left $ TypeError "List in `head` function expected"
            calc Tail (ListExpr (head:tail)) = return $ ListExpr tail
            calc Tail _  = Left $ TypeError "List in `tail` function expected"

eval env (Ident name) = (env, result) where
    result = do
        lambda <- head $ getByName env name
        snd $ eval env lambda

eval env application@(App func arg) = 
    if null $ processApp env application then
        (env, Left $ NotInScope "Non-exhaustive pattern")
    else (env,result) where
        result = do
            lambda <- head $ processApp env application
            snd $ eval env lambda

eval env (Def func patterns body) = (nenv, return None) where
    nenv = setByName env func lambda
    lambda = Lambda patterns body nenv

eval env lambda@(Lambda patterns body closure) = (env, result) where 
    result = 
        if null patterns then
            snd $ eval closure body
        else
            return lambda

eval env (Where (Def func patterns body) helpers) = (nenv, return None) where
    nenv = setByName env func lambda
    lambda = Lambda patterns body (Map.union whereEnv nenv)
    whereEnv = fst $ evalMany env helpers

eval env (Let (Where (Def func patterns body) helpers)) = 
    eval nenv (Where (Def func patterns body) helpers) where
        nenv = Map.delete func env
eval env (Let (Def func patterns body)) = 
    eval nenv (Def func patterns body) where
        nenv = Map.delete func env
eval env _ = (env, Left $ WrongArgument)


processApp :: Env -> Expr -> [Safe Expr]
processApp env (App func arg) = 
    substitute env lambdas arg where
        lambdas = case func of 
            lambda@(Lambda _ _ _) -> [Right lambda]
            (Ident name)          -> getByName env name
            otherwise             -> processApp env func
processApp env (Ident name) = getByName env name
processApp _ _ = [Left $ TypeError "Expected function in application"]


substitute :: Env -> [Safe Expr] -> Expr -> [Safe Expr]
substitute env ((Right (Lambda patterns body closure)) : other) arg = 
    if null patterns then 
        substitute env (processApp closure body ++ other) arg
    else
        if (snd $ evalToWHNF env arg) `match` (head patterns) then
            (lambda' : substitute env other arg) 
        else
            substitute env other arg
    where
        lambda' = return $ Lambda abstr body closure'
        closure' = bindNames closure (zip patterns [value])
        value = Lambda [] arg env
        abstr = tail patterns
substitute env [] _ = []
substitute env _ _ = [Left WrongArgument]


match :: Safe Expr -> Pattern -> Bool
match (Left _) _ = False
match (Right _) (NamePattern name) = True
match (Right _) (WildCardPattern) = True
match (Right (ListExpr [])) (EmptyListPattern) = True
match (Right (ListExpr (head:tail))) (ListPattern headPattern tailPattern) = 
    match (return head) headPattern && match (return $ ListExpr tail) tailPattern
match (Right (PairExpr (first,second))) (PairPattern (firstPattern,secondPattern)) = 
    match (return first) firstPattern && match (return second) secondPattern
match _ _ = False


getByName :: Env -> Name -> [Safe Expr]
getByName env name = case (Map.lookup name env) of
    Nothing    -> [Left $ NotInScope name]
    Just exprs -> exprs


setByName :: Env -> Name -> Expr -> Env
setByName env name expr = case (Map.lookup name env) of
    Nothing    -> Map.insert name [return expr] env
    Just exprs -> Map.insert name (exprs ++ [return expr]) env


bindNames :: Env -> [(Pattern, Expr)] -> Env
bindNames env [] = env
bindNames env ((pattern, expr) : bindings) = Map.union other current where
    other = bindNames env bindings
    current = case pattern of
        NamePattern name      -> setByName env name expr
        WildCardPattern       -> env
        EmptyListPattern      -> env
        PairPattern (p1, p2)  -> bindNames env [ (p1, App (Ident "fst") expr)
                                               , (p2, App (Ident "snd") expr)]
        ListPattern head tail -> bindNames env [ (head, App (Ident "head") expr)
                                               , (tail, App (Ident "tail") expr)]


evalMany :: Env -> [Expr] -> (Env, Safe Expr)
evalMany env [] = (env, return None)
evalMany env ((Def func patterns body):other) = (nenv, return None) where
    currentEnv = setByName env func currentLambda
    currentLambda = Lambda patterns body nenv
    othersEnv = fst $ evalMany currentEnv other
    nenv = othersEnv


evalToWHNF :: Env -> Expr -> (Env, Safe Expr)
evalToWHNF env int@(IntLiteral _) = (env, return int)
evalToWHNF env bool@(BoolLiteral _) = (env, return bool)
evalToWHNF env char@(CharLiteral _) = (env, return char)
evalToWHNF env (Undefined) = (env, (Left UndefinedNameError))

evalToWHNF env (ListExpr list) = (env, Right $ ListExpr list) 
evalToWHNF env (PairExpr (x,y)) = (env, Right $ PairExpr (x,y))

evalToWHNF env (IfThenElse predicate a b) = (env, result) where
    result = do
        condition <- snd $ eval env predicate
        if condition == (BoolLiteral True) then 
            snd $ evalToWHNF env a
        else if condition == (BoolLiteral False) then
            snd $ evalToWHNF env b
        else Left $ TypeError "Boolean condition in `if` expected"

evalToWHNF env (AppBinOp l op r) = (env, result) where
    result = do
        l <- snd $ evalToWHNF env l
        r <- snd $ evalToWHNF env r
        calc l op r where
            calc (IntLiteral a) Add (IntLiteral b) = return $ IntLiteral (a + b)
            calc _ Add _  = Left $ TypeError "Int in + operator expected"
            calc (IntLiteral a) Sub (IntLiteral b) = return $ IntLiteral (a - b)
            calc _ Sub _  = Left $ TypeError "Int in - operator expected"
            calc (IntLiteral a) Mul (IntLiteral b) = return $ IntLiteral (a * b)
            calc _ Mul _  = Left $ TypeError "Int in * operator expected"
            calc (IntLiteral a) Div (IntLiteral b) = return $ IntLiteral (a `div` b)
            calc _ Div _  = Left $ TypeError "Int in `div` operator expected"
            
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
            
            calc (ListExpr xs) Concat (ListExpr ys) = return $ ListExpr (xs ++ ys)
            calc _ Concat _  = Left $ TypeError "Lists in `concat` operator expected"
            calc (x) Push (ListExpr xs) = return $ ListExpr (x : xs)
            calc _ Push _  = Left $ TypeError "Expr and [Expr] in : operator expected"

evalToWHNF env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ evalToWHNF env x
        calc op x where
            calc Neg (IntLiteral a) = return $ IntLiteral (-a)
            calc Neg _  = Left $ TypeError "Int in unary - operator expected"

            calc Not (BoolLiteral a) = return $ BoolLiteral (not a)
            calc Not _  = Left $ TypeError "Bool in not operator expected"

            calc Fst (PairExpr (p1, _)) = return $ p1
            calc Fst _  = Left $ TypeError "Pair in `fst` function expected"
            calc Snd (PairExpr (_, p2)) = return $ p2
            calc Snd _  = Left $ TypeError "Pair in `snd` function expected"
            
            calc Head (ListExpr (head:tail)) = return $ head
            calc Head _  = Left $ TypeError "List in `head` function expected"
            calc Tail (ListExpr (head:tail)) = return $ ListExpr tail
            calc Tail _  = Left $ TypeError "List in `tail` function expected"

evalToWHNF env (Ident name) = (env, result) where
    result = do
        lambda <- head $ getByName env name
        snd $ eval env lambda

evalToWHNF env application@(App func arg) = 
    if null $ processApp env application then
        (env, Left $ NotInScope "Non-exhaustive pattern")
    else (env,result) where
        result = head $ processApp env application

evalToWHNF env lambda@(Lambda patterns body closure) = (env, result) where
    result = return lambda

--foldConst :: Env -> Expr -> Expr
--foldConst env (App (App (Ident "+") (Ident a)) (Ident b)) = 