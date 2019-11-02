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

data BinOp = :+: | :-: | :*: | :/:
            :&&: | :||: | :==: | :>: | :<:
        deriving Show

data UnOp = Neg | Not

data Expr = Ident          Name
          | App            Expr [Expr]
          | AppBinOp       Expr BinOp Expr
          | AppUnOp        UnOp Expr
          | Def            Name [Pattern] Expr

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr] 

          | IntLiteral     Int
          | CharLiteral    Char
          | StringLiteral  String
          | BoolLiteral    Bool
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)

          |                Expr :->: Expr
          | TypeExpr       Type
          | TypeDef        Expr Expr         -- for expressions with ::

        deriving Show 