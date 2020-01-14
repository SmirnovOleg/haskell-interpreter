{-# LANGUAGE NoMonadFailDesugaring #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Interpreter where

import AST
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Runtime
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Debug.Trace
import Data.Functor


eval :: Expr -> SafeT (State Env) Expr
eval int@(IntLiteral _) = return int
eval bool@(BoolLiteral _) = return bool
eval char@(CharLiteral _) = return char

eval Undefined = throwE UndefinedNameError
eval None = return None

eval (ListExpr list) = ListExpr <$> mapM eval list

eval (PairExpr (x, y)) = do
    x' <- eval x
    y' <- eval y
    return $ PairExpr (x', y')

eval (IfThenElse predicate a b) = do
    condition <- eval predicate
    if condition == (BoolLiteral True) then 
        eval a
    else if condition == (BoolLiteral False) then
        eval b
    else throwE $ TypeError "Boolean condition in `if` expected"

eval (AppBinOp l op r) = do
    l <- eval l
    r <- eval r
    except $ Runtime.calcBinOp l op r

eval (AppUnOp op x) = do
    x' <- eval x
    except $ Runtime.calcUnOp op x'

eval (Ident name) = do
    env <- lift get
    (lambda, _) <- except $ head $ getByName env name
    eval lambda

eval app@(App func arg) = do
    env <- lift get
    let safeLambdas = processApplication env app
    if null $ safeLambdas then
        throwE $ NotInScope "Non-exhaustive pattern"
    else do
        (lambda, _) <- except $ head safeLambdas
        eval lambda

eval (Def func patterns body) = do
    let update env = nenv where
            nenv = setByName env func lambda
            lambda = runReader (foldConst $ Lambda patterns body nenv) nenv
    modify update
    return None        

eval lambda@(Lambda patterns body closure) = 
    if null patterns then
        except $ evalState (runExceptT $ eval body) closure
    else
        return lambda

eval (Where (Def func patterns body) helpers) = do
    let update env = nenv where
            nenv = setByName env func lambda
            lambda = runReader (foldConst $ Lambda patterns body closure) closure
            closure = Map.union whereEnv nenv
            whereEnv = evalMany env helpers
    modify update
    return None

eval (Let whereDef@(Where (Def func patterns body) helpers)) = do
    modify (Map.delete func)
    eval whereDef

eval (Let def@(Def func patterns body)) = do
    modify (Map.delete func)
    eval def

eval _ = throwE WrongArgument


processApplication :: Env -> Expr -> [Safe (Expr, Type)]
processApplication env (App func arg) = 
    substitute env lambdas arg where
        lambdas = case func of 
            lambda@(Lambda _ _ _) -> [return (lambda, HUnknown)]
            (Ident name)          -> getByName env name
            otherwise             -> processApplication env func
processApplication env (Ident name) = getByName env name
processApplication _ _ = [Left $ TypeError "Expected function in application"]


substitute :: Env -> [Safe (Expr, Type)] -> Expr -> [Safe (Expr, Type)]
substitute env (Right (Lambda patterns body closure, _):other) arg = 
    if null patterns then 
        substitute env (processApplication closure body ++ other) arg
    else 
        case head patterns of
            NamePattern _ -> lambda' : substitute env other arg
            otherwise     ->
                case runReader (runExceptT $ evalToWHNF arg) env of
                    Left _     -> substitute env other arg
                    Right expr -> 
                        if expr `match` (head patterns) then
                            lambda' : substitute env other arg 
                        else
                            substitute env other arg
        where
            lambda' = return (Lambda abstr body closure', HUnknown)
            closure' = bindNames closure (zip patterns [value])
            value = Lambda [] arg env
            abstr = tail patterns
substitute env (Right (expr,tp):other) arg = substitute env (reduce (expr,tp) : other) arg where
    reduce (expr,tp) = (runReader (runExceptT $ evalToWHNF expr) env) <&> (\x -> (x,tp))
substitute _ [] _ = []
substitute _ (Left e:other) _ = [Left e]


match :: Expr -> Pattern -> Bool
match _ (NamePattern name) = True
match _ (WildCardPattern) = True
match (ListExpr []) (EmptyListPattern) = True
match (ListExpr (head:tail)) (ListPattern headPattern tailPattern) = 
    match head headPattern && match (ListExpr tail) tailPattern
match (PairExpr (first,second)) (PairPattern (firstPattern,secondPattern)) = 
    match first firstPattern && match second secondPattern
match _ _ = False


getByName :: Env -> Name -> [Safe (Expr, Type)]
getByName env name = case Map.lookup name env of
    Nothing    -> [Left $ NotInScope name]
    Just exprs -> exprs


setByName :: Env -> Name -> Expr -> Env 
setByName env name expr =
    case Map.lookup name env of
        Nothing    -> Map.insert name [safelyTypedExpr] env
        Just exprs -> Map.insert name (exprs ++ [safelyTypedExpr]) env
    where 
        safelyTypedExpr = do
            tp <- getTypeForDef env expr name
            return (expr, tp)
            

bindNames :: Env -> [(Pattern, Expr)] -> Env
bindNames env [] = env
bindNames env ((pattern, expr):bindings) = Map.union other current 
    where
        other = bindNames env bindings
        current = case pattern of
            NamePattern name      -> setByName env name expr
            WildCardPattern       -> env
            EmptyListPattern      -> env
            PairPattern (p1, p2)  -> bindNames env [ (p1, App (Ident "fst") expr)
                                                   , (p2, App (Ident "snd") expr)]
            ListPattern head tail -> bindNames env [ (head, App (Ident "head") expr)
                                                   , (tail, App (Ident "tail") expr)]


evalMany :: Env -> [Expr] -> Env 
evalMany env [] = env

evalMany env ((Def func patterns body):other) = nenv 
    where
        curEnv = setByName env func curLambda
        curLambda = runReader (foldConst $ Lambda patterns body nenv) nenv
        nenv = evalMany curEnv other

evalMany env ((Let (Def func patterns body)):other) = nenv
    where
        env' = Map.delete func env
        curEnv = setByName env func curLambda
        curLambda = runReader (foldConst $ Lambda patterns body nenv) nenv
        nenv = evalMany curEnv other

evalMany env _ = env


evalToWHNF :: Expr -> SafeT (Reader Env) Expr
evalToWHNF int@(IntLiteral _) = return int
evalToWHNF bool@(BoolLiteral _) = return bool
evalToWHNF char@(CharLiteral _) = return char
evalToWHNF Undefined = throwE UndefinedNameError

evalToWHNF (ListExpr list) = return $ ListExpr list
evalToWHNF (PairExpr (x,y)) = return $ PairExpr (x,y)

evalToWHNF (IfThenElse predicate a b) = do
    condition <- evalToWHNF predicate
    if condition == (BoolLiteral True) then 
        evalToWHNF a
    else if condition == (BoolLiteral False) then
        evalToWHNF b
    else throwE $ TypeError "Boolean condition in `if` expected"

evalToWHNF (AppBinOp l op r) = do
    l <- evalToWHNF l
    r <- evalToWHNF r
    except $ Runtime.calcBinOp l op r

evalToWHNF (AppUnOp op x) = do
    x <- evalToWHNF x
    except $ Runtime.calcUnOp op x

evalToWHNF (Ident name) = do
    env <- lift ask
    (lambda, _) <- except $ head $ getByName env name
    evalToWHNF lambda

evalToWHNF app@(App func arg) = do
    env <- lift ask
    let safeLambdas = processApplication env app
    if null $ safeLambdas then
        throwE $ NotInScope "Non-exhaustive pattern"
    else do
        (lambda, _) <- except $ head $ safeLambdas
        evalToWHNF lambda

evalToWHNF lambda@(Lambda patterns body closure) =
    if null patterns then
        except $ runReader (runExceptT $ evalToWHNF body) closure
    else
        return lambda

evalToWHNF _ = throwE WrongArgument


foldConst :: Expr -> Reader Env Expr
foldConst (IntLiteral a) = return $ IntLiteral a
foldConst (BoolLiteral a) = return $ BoolLiteral a

foldConst (Ident name) = do
    env <- ask
    case head $ getByName env name of
        Right (lambda, _) -> foldConst lambda
        Left _ -> return $ Ident name

foldConst lambda@(Lambda patterns body closure) = 
    if null patterns then
        return $ runReader (foldConst body) closure
    else 
        return lambda

foldConst app@(App (App (Ident "+") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (IntLiteral x, IntLiteral y) -> return $ IntLiteral (x + y)
        otherwise -> return $ app

foldConst app@(App (App (Ident "-") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (IntLiteral x, IntLiteral y) -> return $ IntLiteral (x - y)
        otherwise -> return $ app

foldConst app@(App (App (Ident "*") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (IntLiteral x, IntLiteral y) -> return $ IntLiteral (x * y)
        otherwise -> return $ app

foldConst app@(App (App (Ident "`div`") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (IntLiteral x, IntLiteral y) -> return $ IntLiteral (x `div` y)
        otherwise -> return $ app

foldConst app@(App (App (Ident "&&") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (BoolLiteral x, BoolLiteral y) -> return $ BoolLiteral (x && y)
        otherwise -> return $ app

foldConst app@(App (App (Ident "||") a) b) = do
    a' <- foldConst a
    b' <- foldConst b
    case (a', b') of
        (BoolLiteral x, BoolLiteral y) -> return $ BoolLiteral (x || y)
        otherwise -> return $ app

foldConst app@(App (Ident "not") a) = do
    a' <- foldConst a
    case a' of
        BoolLiteral x -> return $ BoolLiteral (not x)
        otherwise -> return $ app

foldConst app@(App (Ident "-") a) = do
    a' <- foldConst a
    case a' of
        IntLiteral x -> return $ IntLiteral (-x)
        otherwise -> return $ app

foldConst expr = return expr


getTypeForDef :: Env -> Expr -> Name -> Safe Type
getTypeForDef env expr name = do
    (_, tenv, _) <- execStateT (inferType expr) (Runtime.tvs, [], env)
    tp <- maybeToSafe $ List.lookup expr tenv
    case List.lookup (Ident name) tenv of
        Nothing -> return tp
        Just tv -> do
            tenv' <- unifyTypes (tv, tp) tenv
            maybeToSafe $ List.lookup expr tenv'


getType :: Env -> Expr -> Safe Type
getType env expr = do
    (_, tenv, _) <- execStateT (inferType expr) (Runtime.tvs, [], env)
    maybeToSafe $ List.lookup expr tenv


inferType :: Expr -> StateT (Tvs, TEnv, Env) Safe ()
inferType expr@(IntLiteral _) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> put (tvs, (expr, HInt) : tenv, env)
        Just _  -> return ()

inferType expr@(BoolLiteral _) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> put (tvs, (expr, HBool) : tenv, env)
        Just _  -> return ()

inferType expr@(CharLiteral _) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> put (tvs, (expr, HChar) : tenv, env)
        Just _  -> return ()

inferType expr@(PairExpr (a, b)) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> do
            (tvs', tenv', _) <- lift $ execStateT (inferType a) (tvs, tenv, env)
            ta <- lift $ maybeToSafe $ List.lookup a tenv'
            (tvs'', tenv'', _) <- lift $ execStateT (inferType b) (tvs', tenv', env)
            tb <- lift $ maybeToSafe $ List.lookup b tenv''
            put (tvs'', (expr, HPair ta tb) : tenv'', env)
        Just _  -> return ()

inferType expr@(ListExpr []) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> put (tvs, (expr, HList (HVar "_")) : tenv, env)
        Just _  -> return ()

inferType expr@(ListExpr (x:xs)) = do
    (tvs, tenv, env) <- get
    case List.lookup expr tenv of
        Nothing -> do
            (tvs', tenv', _) <- lift $ execStateT (inferType x) (tvs, tenv, env)
            tx <- lift $ maybeToSafe $ List.lookup x tenv'
            (tvs'', tenv'', _) <- lift $ execStateT (inferType $ ListExpr xs) (tvs', tenv', env)
            (HList txs) <- lift $ maybeToSafe $ List.lookup (ListExpr xs) tenv''
            if areTypesCompatible txs tx then do 
                tenv''' <- lift $ unifyTypes (tx, txs) tenv''
                tx' <- lift $ maybeToSafe $ List.lookup x tenv'''
                put (tvs'', (expr, HList tx') : tenv''', env)
            else
                lift $ Left $ TypeError "Incompatible types"
        Just _  -> return ()

inferType expr@(IfThenElse predicate thenExpr elseExpr) = do
    st <- get
    (tvs, tenv, env) <- lift $ execStateT (inferType predicate) st
    tpredicate <- lift $ maybeToSafe $ List.lookup predicate tenv
    if tpredicate == HBool then do
        (tvs', tenv', _) <- lift $ execStateT (inferType thenExpr) (tvs, tenv, env)
        ta <- lift $ maybeToSafe $ List.lookup thenExpr tenv'
        (tvs'', tenv'', _) <- lift $ execStateT (inferType elseExpr) (tvs', tenv', env)
        tb <- lift $ maybeToSafe $ List.lookup elseExpr tenv''
        if areTypesCompatible ta tb then do
            tenv''' <- lift $ unifyTypes (ta, tb) tenv''
            ta' <- lift $ maybeToSafe $ List.lookup thenExpr tenv'''
            put (tvs'', (expr, ta') : tenv''', env)
        else
            lift $ Left $ TypeError "Branches `then` and `else` have incompatible types"
    else 
        lift $ Left $ TypeError "Expected Bool in `if` condition"

inferType expr@(Ident name) = do
    (tv:tvs, tenv, env) <- get
    let safeLambda = head $ getByName env name
    case safeLambda of
        Left (NotInScope _) -> 
            case List.lookup expr tenv of
                Nothing -> put (tvs, (expr, HVar tv) : tenv, env)
                Just _  -> return ()
        Right _ -> do
            (_, tp) <- lift $ safeLambda
            put (tv:tvs, (expr, tp) : tenv, env)
        Left error -> lift $ Left error

inferType expr@(Lambda [] body closure) = do
    st <- get
    (tvs, tenv, env) <- lift $ execStateT (inferType body) st
    tlambda <- lift $ maybeToSafe $ List.lookup body tenv
    let tenv' = (expr, tlambda) : tenv
    put (tvs, tenv', env)

inferType expr@(Lambda (p:ps) body closure) = do
    (tvs, tenv, env) <- get
    let (tenv', env') = execState (prepareForPatternTypeInference p) (tenv, env)
    (tvs'', tenv'', _) <- lift $ execStateT (inferType (Lambda ps body closure)) (tvs, tenv', env')
    tlambda <- lift $ maybeToSafe $ List.lookup (Lambda ps body closure) tenv''
    (tp, (tvs''', tenv''')) <- lift $ runStateT (getPatternType p) (tvs'', tenv'')
    put (tvs''', (expr, HArrow tp tlambda) : tenv''', env)

inferType expr@(App func arg) = do 
    (tv:tvs, tenv, env) <- get
    (tvs', tenv', _) <- lift $ execStateT (inferType arg) (tvs, tenv, env)
    targ <- lift $ maybeToSafe $ List.lookup arg tenv'
    case func of
        Ident name -> do
            let safeLambda = head $ getByName env name
            case safeLambda of
                Left (NotInScope _) -> 
                    case List.lookup func tenv' of
                        Just tfunc -> do 
                            tenv'' <- lift $ unifyTypes (HArrow targ (HVar tv), tfunc) tenv'
                            let mt = List.lookup func tenv''
                            (HArrow targ' tfunc') <- lift $ maybeToSafe mt
                            put (tvs', (expr, tfunc') : tenv'', env)
                        Nothing -> do
                            let tenv'' = (func, HArrow targ (HVar tv)) : tenv'
                            put (tvs', (expr, HVar tv) : tenv'', env)
                Right _ -> do
                    (_, tfunc) <- lift $ safeLambda
                    let tenv'' = (Ident name, tfunc) : tenv'
                    tenv''' <- lift $ unifyTypes (HArrow targ (HVar tv), tfunc) tenv''
                    let mt = List.lookup func tenv'''
                    (HArrow targ' tfunc') <- lift $ maybeToSafe mt
                    put (tvs', (expr, tfunc') : tenv''', env)
                Left error -> 
                    lift $ Left error
        Lambda _ _ _ -> do
            (tvs'', tenv'', _) <- lift $ execStateT (inferType func) (tvs', tenv', env)
            (HArrow targ' tres') <- lift $ maybeToSafe $ List.lookup func tenv''
            if areTypesCompatible targ' targ then do 
                tenv''' <- lift $ unifyTypes (targ', targ) tenv''
                (HArrow targ'' tres'') <- lift $ maybeToSafe $ List.lookup func tenv'''
                put (tvs'', (expr, tres'') : tenv''', env)
            else 
                lift $ Left $ TypeError "Incompatible types"
        App _ _  -> do 
            (tvs'', tenv'', _) <- lift $ execStateT (inferType func) (tvs', tenv', env)
            tfunc <- lift $ maybeToSafe $ List.lookup func tenv''
            tenv''' <- lift $ unifyTypes (tfunc, HArrow targ (HVar tv)) tenv''
            (HArrow targ' tres') <- lift $ maybeToSafe $ List.lookup func tenv'''
            put (tvs'', (expr, tres') : tenv''', env)
        otherwise -> do
            lift $ Left $ TypeError "Function or lambda abstraction in application expected"

inferType _ = lift $ Left TypeInferenceError


prepareForPatternTypeInference :: Pattern -> State (TEnv, Env) ()
prepareForPatternTypeInference pattern = do
    (tenv, env) <- get
    case pattern of 
        NamePattern p -> do
            let tenv' = case List.lookup (Ident p) tenv of
                            Just tp -> List.delete (Ident p, tp) tenv
                            Nothing -> tenv
            let env' = Map.delete p env
            put (tenv', env')
        ListPattern p ps -> do
            let (tenv', env') = execState (prepareForPatternTypeInference p) (tenv, env)
            let (tenv'', env'') = execState (prepareForPatternTypeInference ps) (tenv', env')
            put (tenv'', env'')
        PairPattern (a, b) -> do
            let (tenv', env') = execState (prepareForPatternTypeInference a) (tenv, env)
            let (tenv'', env'') = execState (prepareForPatternTypeInference b) (tenv', env')
            put (tenv'', env'')
        otherwise -> return ()


getPatternType :: Pattern -> StateT (Tvs, TEnv) Safe Type
getPatternType pattern = do
    (tv:tvs, tenv) <- get
    case pattern of
        NamePattern p -> do
            case List.lookup (Ident p) tenv of
                Nothing -> do
                    put (tvs, (Ident p, HVar tv) : tenv)
                    return $ HVar tv
                Just tp -> return tp
        ListPattern p ps -> do
            (tp, (tvs', tenv')) <- lift $ runStateT (getPatternType p) (tvs, tenv)
            (tps, (tvs'', tenv'')) <- lift $ runStateT (getPatternType ps) (tvs', tenv')
            if areTypesCompatible tp tps then do
                tenv''' <- lift $ unifyTypes (tp, tps) tenv''
                put (tvs'', tenv''')
                return $ HList tp
            else
                lift $ Left $ TypeError "Incorrect list pattern"
        PairPattern (a, b) -> do
            (ta, (tvs', tenv')) <- lift $ runStateT (getPatternType a) (tvs, tenv)
            (tb, (tvs'', tenv'')) <- lift $ runStateT (getPatternType b) (tvs', tenv')
            put (tvs'', tenv'')
            return $ HPair ta tb
        EmptyListPattern -> return $ HList (HVar tv)
        WildCardPattern  -> return $ HVar tv


unifyTypes :: (Type, Type) -> TEnv -> Safe TEnv
unifyTypes (HInt, HInt) tenv = return tenv
unifyTypes (HBool, HBool) tenv = return tenv
unifyTypes (HChar, HChar) tenv = return tenv
unifyTypes (HVar tv, t) tenv = return $ List.map (substituteTypeVar tv t) tenv
unifyTypes (t, HVar tv) tenv = return $ List.map (substituteTypeVar tv t) tenv
unifyTypes (HArrow targ tfunc, HArrow targ' tfunc') tenv = do
    tenv' <- unifyTypes (targ, targ') tenv
    unifyTypes (tfunc, tfunc') tenv'
unifyTypes (HPair ta tb, HPair ta' tb') tenv = do
    tenv' <- unifyTypes (ta, ta') tenv
    unifyTypes (tb, tb') tenv'
unifyTypes (HList tx, HList tx') tenv = unifyTypes (tx, tx') tenv
unifyTypes _ _ = Left $ TypeError "Non-unifying types"


substituteTypeVar :: String -> Type -> (Expr, Type) -> (Expr, Type)
substituteTypeVar _ _ (e, HInt) = (e, HInt)
substituteTypeVar _ _ (e, HBool) = (e, HBool)
substituteTypeVar _ _ (e, HChar) = (e, HChar)
substituteTypeVar tv t (e, HVar tv') = 
    if tv == tv' then (e, t) else (e, HVar tv')
substituteTypeVar tv t (e, HArrow targ tres) = 
    (e, HArrow (snd $ substituteTypeVar tv t (e, targ)) (snd $ substituteTypeVar tv t (e, tres)))
substituteTypeVar tv t (e, HPair ta tb) = 
    (e, HPair (snd $ substituteTypeVar tv t (e, ta)) (snd $ substituteTypeVar tv t (e, tb)))
substituteTypeVar tv t (e, HList tx) = 
    (e, HList (snd $ substituteTypeVar tv t (e, tx)))
substituteTypeVar _ _ tp = tp


areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible HInt _ = True
areTypesCompatible HBool _ = True
areTypesCompatible HChar _ = True
areTypesCompatible (HVar _) _ = True
areTypesCompatible (HArrow targ tres) (HArrow targ' tres')
  = areTypesCompatible targ targ' && areTypesCompatible tres tres'
areTypesCompatible (HPair ta tb) (HPair ta' tb')
  = areTypesCompatible ta ta' && areTypesCompatible tb tb'
areTypesCompatible (HList tx) (HList tx')
  = areTypesCompatible tx tx'
areTypesCompatible _ _ = False


maybeToRight :: e -> Maybe a -> Either e a 
maybeToRight e (Just a) = Right a
maybeToRight e Nothing  = Left e


maybeToSafe :: Maybe a -> Safe a
maybeToSafe = maybeToRight TypeInferenceError