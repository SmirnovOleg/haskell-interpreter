module Main where

import Lib
import AST

main :: IO ()
main = testFunc

factorial :: Expr
factorial = 
        (
            Def
            (NameLiteral "factorial")
            [(NameLiteral "n")]
            (
                IfThenElse
                (Eq (NameLiteral "n") (IntLiteral 0))
                (IntLiteral 1) 
                (
                    Just 
                    (
                        Mul
                        (NameLiteral "n") 
                        (
                            Func 
                            (NameLiteral "factorial")
                            [Sub (NameLiteral "n") (IntLiteral 1)]
                        )
                    )
                )
            )
        )

fibonacci :: Expr
fibonacci = 
    (
        Def 
        (NameLiteral "fibonacci") 
        [NameLiteral "n"] 
        (
            IfThenElse 
            (
                Or 
                (Eq (NameLiteral "n") (IntLiteral 0))
                (Eq (NameLiteral "n") (IntLiteral 1))
            ) 
            (IntLiteral 1) 
            (
                Just 
                (
                    Add 
                    (
                        Func 
                        (NameLiteral "fibonacci") 
                        [Sub (NameLiteral "n") (IntLiteral 1)] 
                    )
                    (
                        Func 
                        (NameLiteral "fibonacci") 
                        [Sub (NameLiteral "n") (IntLiteral 2)]
                    )
                )
            )
        )
    )