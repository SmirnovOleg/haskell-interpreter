module Main where

import AST

main :: IO ()
main = do
    putStrLn "Hello World!"

-- factorial n = if n == 0 then 1 else n * fact (n - 1) 
factorial :: Expr
factorial = 
        (
            Def ("factorial") [(NamePattern "n")]
            (
                IfThenElse
                (AppBinOp Eq (Ident "n")  (IntLiteral 0))
                (IntLiteral 1)
                (AppBinOp Mul (Ident "n") (App (Ident "factorial") (AppBinOp Mul (Ident "n") (IntLiteral 1))))
            )
        )
