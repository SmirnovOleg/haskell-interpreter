module AST where

type Name = String

data Type = Int
          | Bool
          | Char
        deriving Show

data Pattern = NamePattern  Name
             | ListPattern  [Name]
             | PairPattern  (Name, Name)
            deriving Show

data Expr = Var            Name
          | App            Expr [Expr]
          | Def            Name [Pattern] Expr

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr] 

          | IntLiteral     Int
          | CharLiteral    Char
          | StringLiteral  String
          | BoolLiteral    Bool
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)

          |                Expr :+: Expr
          |                Expr :-: Expr
          |                Expr :*: Expr
          |                Expr :/:Expr
          |                Expr :&&: Expr
          |                Expr :||: Expr
          |                Expr :==: Expr
          |                Expr :<: Expr
          |                Expr :>: Expr 
          | Neg            Expr
          | Not            Expr

          |                Expr :->: Expr
          | TypeExpr       Type
          | TypeDef        Expr Expr         -- for expressions with ::

        deriving Show 