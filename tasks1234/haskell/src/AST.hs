module AST where

data Type =   Int
            | Bool
            | Char
            deriving Show

data Pattern = NamePattern  String
            | ListPattern   [String]
            | PairPattern   (String, String)
            deriving Show

data Expr =   Name           String
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

            | Call           {func :: Expr, params :: [Expr]}
            | IfThenElse     {condition :: Expr, statement :: Expr, elseStatement :: (Maybe Expr)}
            | Def            {name :: (Maybe Expr), args :: [Pattern], body :: Expr}
            |                Expr :->: Expr
            | TypeExpr       Type
            | TypeDef        Expr Expr         -- for expressions with ::
            deriving Show 