module Main where

import AST

main :: IO ()
main = testFunc


-- factorial n = if n == 0 then 1 else n * fact (n - 1) 
factorial :: Expr
factorial = 
        (
            Def
            (Just (Name "factorial"))
            [(NamePattern "n")]
            (
                IfThenElse
                (Eq (Name "n") (IntLiteral 0))
                (IntLiteral 1) 
                (
                    Just 
                    (
                        Mul
                        (Name "n") 
                        (
                            Call 
                            (Name "factorial")
                            [Sub (Name "n") (IntLiteral 1)]
                        )
                    )
                )
            )
        )

-- fibonacci n = if (n == 0) || (n == 1) then 1 else fibonacci (n - 1) + fibonacci (n - 2) 
fibonacci :: Expr
fibonacci = 
    (
        Def 
        (Just (Name "fibonacci")) 
        [(NamePattern "n")] 
        (
            IfThenElse 
            (
                Or 
                (Eq (Name "n") (IntLiteral 0))
                (Eq (Name "n") (IntLiteral 1))
            ) 
            (IntLiteral 1) 
            (
                Just 
                (
                    Add 
                    (
                        Call 
                        (Name "fibonacci") 
                        [Sub (Name "n") (IntLiteral 1)] 
                    )
                    (
                        Call 
                        (Name "fibonacci") 
                        [Sub (Name "n") (IntLiteral 2)]
                    )
                )
            )
        )
    )