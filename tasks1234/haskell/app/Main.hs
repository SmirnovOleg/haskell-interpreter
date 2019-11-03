module Main where

import AST

main :: IO ()
main = do
    putStrLn "Hello World!"


{- factorial n = if n == 0 then 1 else n * fact (n - 1) 
factorial :: Expr
factorial = 
        (
            Def ("factorial") [(NamePattern "n")]
            (
                IfThenElse
                ((Var "n") :==: (IntLiteral 0))
                (IntLiteral 1)
                ((Var "n") :*: (App (Var "factorial") [((Var "n") :-: (IntLiteral 1))]))
            )
        )-}