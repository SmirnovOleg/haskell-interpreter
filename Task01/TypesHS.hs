module TypesHs where

data OrdOp = Eq
           | Lt
           | Gt 
           deriving Show

data LogicOp = And
             | Or
             | Not  
             deriving Show

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
          | Negate         Expr
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)
          | Add            Expr Expr
          | Sub            Expr Expr
          | Mul            Expr Expr
          | Frac           Expr Expr
          | Lambda         Expr
          | Func           {nameFunc :: Expr, params :: [Expr]}
          | Ord            Expr OrdOp Expr
          | Logic          Expr LogicOp Expr   
          | IfThenElse     {cond :: Expr, thenExpr :: Expr, elseExpr :: (Maybe Expr)}   
          | KeyWords       KeyWords Expr Expr           
          | Def            {nameFunc :: Expr, params :: [Expr], bodyFunc :: Expr}
          |                Expr :->: Expr
          | TypeExpr       Type
          | TypeDef        Expr Expr
          deriving Show

fact :: Expr
fact  = 
        (
            Def
            (NameLiteral "factorial")
            [NameLiteral "n"]
            (
                IfThenElse 
                (Ord (NameLiteral "n") Eq (IntLiteral 0)) 
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
            (Logic (Ord (NameLiteral "n") Eq (IntLiteral 0)) Or (Ord (NameLiteral "n") Eq (IntLiteral 1))) 
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