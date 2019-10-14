module AST where

data Type =   Int
            | Bool
            | Char
            deriving Show

data Expr =   NullLiteral
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
            | Div            Expr Expr
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
            | Def            {name :: Expr, args :: [Expr], body :: Expr}
            |                Expr :->: Expr
            | TypeExpr       Type
            | TypeDef        Expr Expr         -- for expressions with ::
            deriving Show 
