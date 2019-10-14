module AST where

data KeyWords = Where
                | LetIn
                | CaseOf
                deriving Show

data Type = Int
            | Bool
            | Char
            deriving Show

data Expr = NullLiteral
            | NameLiteral    String
            | IntLiteral     Int
            | CharLiteral    Char
            | StringLiteral  String
            | BoolLiteral    Bool
            | ListExpr       [Expr]
            | PairExpr       (Expr, Expr)

            | Add            Expr Expr
            | Sub            Expr Expr
            | Mul            Expr Expr
            | Div           Expr Expr
            | Neg            Expr

            | Not            Expr
            | And            Expr Expr
            | Or             Expr Expr

            | Eq             Expr Expr
            | Ls             Expr Expr
            | Gt             Expr Expr 

            | Lambda         Expr
            | Func           {name :: Expr, args :: [Expr]}
            | IfThenElse     {condition :: Expr, statement :: Expr, elseStatement :: (Maybe Expr)}
            | KeyWords       KeyWords Expr Expr           
            | Def            {name :: Expr, args :: [Expr], body :: Expr}
            |                Expr :->: Expr
            | TypeExpr       Type
            | TypeDef        Expr Expr
            deriving Show

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