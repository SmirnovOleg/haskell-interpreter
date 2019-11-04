module AST where

type Name = String

data Type = Int
          | Bool
          | Char
        deriving (Show , Ord, Eq) 

data Pattern = NamePattern  Name
             | ListPattern  [Name]
             | PairPattern  (Name, Name)
            deriving (Show , Ord, Eq) 

data BinOp = Add | Sub | Mul | Div 
            | And | Or | Eq | Gt | Lt
        deriving (Show , Ord, Eq) 

data UnOp = Neg | Not
        deriving (Show , Ord, Eq) 

data Expr = Ident          Name
          | App            Expr Expr
          | AppBinOp       BinOp Expr  Expr
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
        deriving (Show , Ord, Eq) 