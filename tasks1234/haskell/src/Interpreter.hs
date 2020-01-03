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
        calcBinOp l op r

eval env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ eval env x
        calcUnOp op x

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
    lambda = foldConst nenv (Lambda patterns body nenv)

eval env lambda@(Lambda patterns body closure) = (env, result) where 
    result = 
        if null patterns then
            snd $ eval closure body
        else
            return lambda

eval env (Where (Def func patterns body) helpers) = (nenv, return None) where
    nenv = setByName env func lambda
    closure = Map.union whereEnv nenv
    lambda = foldConst closure (Lambda patterns body closure)
    whereEnv = fst $ evalMany env helpers

eval env (Let (Where (Def func patterns body) helpers)) = 
    eval nenv (Where (Def func patterns body) helpers) where
        nenv = Map.delete func env
eval env (Let (Def func patterns body)) = 
    eval nenv (Def func patterns body) where
        nenv = Map.delete func env
eval env _ = (env, Left $ WrongArgument)


calcBinOp :: Expr -> BinOp -> Expr -> Safe Expr
calcBinOp (IntLiteral a) Add (IntLiteral b) = return $ IntLiteral (a + b)
calcBinOp _ Add _  = Left $ TypeError "Int in + operator expected"
calcBinOp (IntLiteral a) Sub (IntLiteral b) = return $ IntLiteral (a - b)
calcBinOp _ Sub _  = Left $ TypeError "Int in - operator expected"
calcBinOp (IntLiteral a) Mul (IntLiteral b) = return $ IntLiteral (a * b)
calcBinOp _ Mul _  = Left $ TypeError "Int in * operator expected"
calcBinOp (IntLiteral a) Div (IntLiteral b) = return $ IntLiteral (a `div` b)
calcBinOp _ Div _  = Left $ TypeError "Int in `div` operator expected"

calcBinOp (BoolLiteral a) And (BoolLiteral b) = return $ BoolLiteral (a && b)
calcBinOp _ And _  = Left $ TypeError "Bool in && operator expected"
calcBinOp (BoolLiteral a) Or (BoolLiteral b) = return $ BoolLiteral (a || b)
calcBinOp _ Or _  = Left $ TypeError "Bool in || operator expected"

calcBinOp (IntLiteral a) Eq (IntLiteral b) = return $ BoolLiteral (a == b)
calcBinOp (BoolLiteral a) Eq (BoolLiteral b) = return $ BoolLiteral (a == b)
calcBinOp _ Eq _  = Left $ TypeError "Int or Bool in == operator expected"
calcBinOp (IntLiteral a) Ls (IntLiteral b) = return $ BoolLiteral (a < b)
calcBinOp _ Ls _  = Left $ TypeError "Int in < operator expected"
calcBinOp (IntLiteral a) Gt (IntLiteral b) = return $ BoolLiteral (a > b)
calcBinOp _ Gt _  = Left $ TypeError "Int in > operator expected"

calcBinOp (ListExpr xs) Concat (ListExpr ys) = return $ ListExpr (xs ++ ys)
calcBinOp _ Concat _  = Left $ TypeError "Lists in `concat` operator expected"
calcBinOp (x) Push (ListExpr xs) = return $ ListExpr (x : xs)
calcBinOp _ Push _  = Left $ TypeError "Expr and [Expr] in : operator expected"


calcUnOp :: UnOp -> Expr -> Safe Expr
calcUnOp Neg (IntLiteral a) = return $ IntLiteral (-a)
calcUnOp Neg _  = Left $ TypeError "Int in unary - operator expected"

calcUnOp Not (BoolLiteral a) = return $ BoolLiteral (not a)
calcUnOp Not _  = Left $ TypeError "Bool in not operator expected"

calcUnOp Fst (PairExpr (p1, _)) = return $ p1
calcUnOp Fst _  = Left $ TypeError "Pair in `fst` function expected"
calcUnOp Snd (PairExpr (_, p2)) = return $ p2
calcUnOp Snd _  = Left $ TypeError "Pair in `snd` function expected"

calcUnOp Head (ListExpr (head:tail)) = return $ head
calcUnOp Head _  = Left $ TypeError "List in `head` function expected"
calcUnOp Tail (ListExpr (head:tail)) = return $ ListExpr tail
calcUnOp Tail _  = Left $ TypeError "List in `tail` function expected"


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
    currentLambda = foldConst nenv (Lambda patterns body nenv)
    othersEnv = fst $ evalMany currentEnv other
    nenv = othersEnv
evalMany env ((Let (Def func patterns body)):other) = (nenv, return None) where
    env' = Map.delete func env
    currentEnv = setByName env' func currentLambda
    currentLambda = foldConst nenv (Lambda patterns body nenv)
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
        calcBinOp l op r

evalToWHNF env (AppUnOp op x) = (env, result) where
    result = do
        x <- snd $ evalToWHNF env x
        calcUnOp op x

evalToWHNF env (Ident name) = (env, result) where
    result = do
        lambda <- head $ getByName env name
        snd $ evalToWHNF env lambda

evalToWHNF env application@(App func arg) = 
    if null $ processApp env application then
        (env, Left $ NotInScope "Non-exhaustive pattern")
    else (env,result) where
        result = do
            lambda <- head $ processApp env application
            snd $ evalToWHNF env lambda

evalToWHNF env lambda@(Lambda patterns body closure) = (env, result) where
    result = 
        if null patterns then
            snd $ evalToWHNF closure body
        else
            return lambda


foldConst :: Env -> Expr -> Expr
foldConst env (IntLiteral a) = IntLiteral a
foldConst env (BoolLiteral a) = BoolLiteral a

foldConst env (Ident name) =
    case head $ getByName env name of
        Right lambda -> foldConst env lambda
        Left _ -> Ident name
foldConst env lambda@(Lambda patterns body closure) = 
    if null patterns then foldConst closure body else lambda

foldConst env app@(App (App (Ident "+") a) b) = 
    case (foldConst env a, foldConst env b) of
        (IntLiteral a', IntLiteral b') -> IntLiteral (a' + b')
        otherwise -> app
foldConst env app@(App (App (Ident "-") a) b) = 
    case (foldConst env a, foldConst env b) of
        (IntLiteral a', IntLiteral b') -> IntLiteral (a' - b')
        otherwise -> app
foldConst env app@(App (App (Ident "*") a) b) = 
    case (foldConst env a, foldConst env b) of
        (IntLiteral a', IntLiteral b') -> IntLiteral (a' * b')
        otherwise -> app
foldConst env app@(App (App (Ident "`div`") a) b) = 
    case (foldConst env a, foldConst env b) of
        (IntLiteral a', IntLiteral b') -> IntLiteral (a' `div` b')
        otherwise -> app

foldConst env app@(App (App (Ident "&&") a) b) = 
    case (foldConst env a, foldConst env b) of
        (BoolLiteral a', BoolLiteral b') -> BoolLiteral (a' && b')
        otherwise -> app
foldConst env app@(App (App (Ident "||") a) b) = 
    case (foldConst env a, foldConst env b) of
        (BoolLiteral a', BoolLiteral b') -> BoolLiteral (a' || b')
        otherwise -> app
foldConst env app@(App (Ident "not") a) = 
    case (foldConst env a) of
        BoolLiteral a' -> BoolLiteral (not a')
        otherwise -> app
foldConst env app@(App (Ident "-") a) = 
    case (foldConst env a) of
        IntLiteral a' -> IntLiteral (-a')
        otherwise -> app

foldConst _ expr = expr


getTypeByName :: Name -> Env -> [Name] -> TypedEnv -> Safe ([Name], TypedEnv)
getTypeByName name env tvs tenv = do
    lambda <- head $ getByName env name
    (tvs', tenv') <- inferType lambda tvs tenv
    let Just tlambda = Map.lookup lambda tenv'
    return (tvs', Map.insert (Ident name) tlambda tenv')


inferType :: Expr -> [Name] -> TypedEnv -> Safe ([Name], TypedEnv)
inferType expr@(IntLiteral _) tvs tenv =
    case Map.lookup expr tenv of
        Nothing -> return (tvs, Map.insert expr HInt tenv)
        Just _  -> return (tvs, tenv)

inferType expr@(Ident name) typeVars@(tv:tvs) tenv =
    case Map.lookup expr tenv of
        Nothing -> return (tvs, Map.insert expr (HVar tv) tenv)
        Just _  -> return (typeVars, tenv)

inferType expr@(Lambda [] body closure) typeVars tenv = do
    (tvs', tenv') <- inferType body typeVars tenv
    let Just tlambda = Map.lookup body tenv'
    return (tvs', Map.insert expr tlambda tenv')

inferType expr@(Lambda ((NamePattern p) : ps) body closure) typeVars@(tv:tvs) tenv = do
    let tclosure = Map.delete (Ident p) tenv
    (tvs', tenv') <- inferType (Lambda ps body closure) tvs tclosure
    let Just tlambda = Map.lookup (Lambda ps body closure) tenv'
    case Map.lookup (Ident p) tenv' of
        Nothing -> do
            let tenv'' = Map.insert (Ident p) (HVar tv) tenv'
            return (tvs', Map.insert expr (HArrow (HVar tv) tlambda) tenv'')
        Just tp -> return (tvs', Map.insert expr (HArrow tp tlambda) tenv')

inferType expr@(App func arg) typeVars@(tv:tvs) tenv = do 
    (tvs', tenv') <- inferType arg tvs tenv
    let Just targ = Map.lookup arg tenv'
    case func of
        Ident _ ->
            case Map.lookup func tenv' of
                Just tfunc -> do 
                    tenv'' <- unifyTypes (HArrow targ (HVar tv), tfunc) tenv'
                    let Just (HArrow targ' tfunc') = Map.lookup func tenv''
                    return (tvs', Map.insert expr tfunc' tenv'')
                Nothing -> do
                    let tenv'' = Map.insert func (HArrow targ (HVar tv)) tenv'
                    return (tvs', Map.insert expr (HVar tv) tenv'')
        Lambda _ _ _ -> do
            (tvs'', tenv'') <- inferType func tvs' tenv'
            let Just (HArrow targ' tres') = Map.lookup func tenv''
            if areTypesCompatible targ' targ then do 
                tenv''' <- unifyTypes (targ', targ) tenv''
                let Just (HArrow targ'' tres'') = Map.lookup func tenv'''
                return (tvs'', Map.insert expr tres'' tenv''')
            else 
                Left $ TypeError "Incompatible types"
        App _ _  -> do 
            (tvs'', tenv'') <- inferType func tvs' tenv'
            let Just tfunc = Map.lookup func tenv''
            tenv''' <- unifyTypes (tfunc, HArrow targ (HVar tv)) tenv''
            let Just (HArrow targ' tres') = Map.lookup func tenv'''
            return (tvs'', Map.insert expr tres' tenv''')


unifyTypes :: (Type, Type) -> TypedEnv -> Safe TypedEnv
unifyTypes (HInt, HInt) tenv = return tenv
unifyTypes (HVar tv, t) tenv = return $ Map.map (substituteTypeVar tv t) tenv
unifyTypes (t, HVar tv) tenv = return $ Map.map (substituteTypeVar tv t) tenv
unifyTypes (HArrow targ tfunc, HArrow targ' tfunc') tenv = do 
    tenv' <- unifyTypes (targ, targ') tenv
    unifyTypes (tfunc, tfunc') tenv'
unifyTypes (t, t') _ = Left $ TypeError "Non-unifying types"


substituteTypeVar :: String -> Type -> Type -> Type
substituteTypeVar _ _ HInt = HInt
substituteTypeVar tv t (HVar tv') = 
    if tv == tv' then t else HVar tv'
substituteTypeVar tv t (HArrow targ tres) = 
    HArrow (substituteTypeVar tv t targ) (substituteTypeVar tv t tres)


areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible HInt _ = True
areTypesCompatible (HVar _) _ = True
areTypesCompatible (HArrow targ tres) (HArrow targ' tres')
  = areTypesCompatible targ targ' && areTypesCompatible tres tres'
areTypesCompatible _ _ = False