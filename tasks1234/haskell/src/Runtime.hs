module Runtime where

import AST
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

builtInFunctions :: [(Name, [Safe Expr])]
builtInFunctions =  [ ("+", [createBinOpLambda Add])
                    , ("-", [createBinOpLambda Sub])
                    , ("*", [createBinOpLambda Mul])
                    , ("`div`", [createBinOpLambda Div])
                    , ("&&", [createBinOpLambda And])
                    , ("||", [createBinOpLambda Or])
                    , ("==", [createBinOpLambda Eq])
                    , (">", [createBinOpLambda Gt])
                    , ("<", [createBinOpLambda Ls])
                    , ("concat", [createBinOpLambda Concat])
                    , (":", [createBinOpLambda Push])
                    , ("neg", [createUnOpLambda Neg])
                    , ("not", [createUnOpLambda Not])
                    , ("fst", [createUnOpLambda Fst])
                    , ("snd", [createUnOpLambda Snd])
                    , ("head", [createUnOpLambda Head])
                    , ("tail", [createUnOpLambda Tail]) ]

runtime :: Env
runtime = Map.fromList builtInFunctions

tvs :: [Name]
tvs = map (\x -> init $ tail $ show x) "abcdefghijklmnopqrstuvwxyz"

typedRuntime :: TypedEnv
typedRuntime = Map.fromList [ (Ident "+", HArrow HInt (HArrow HInt HInt))
                            , (Ident "-", HArrow HInt (HArrow HInt HInt)) ]