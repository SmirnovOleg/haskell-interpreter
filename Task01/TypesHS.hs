module TypesHs where

data OrdOp = Eq
           | Lt
           | Gt 

data LogicOp = And
             | Or
             | Not  

data KeyWords = Where
              | LetIn
              | CaseOf

data Type = Int
          | Bool
          | Char

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
          | Mul            Expr Expr
          | Frac           Expr Expr
          | Lambda         Expr
          | Ord            Expr OrdOp Expr
          | Logic          Expr LogicOp Expr   
          | IfThenClause   Expr Expr (Maybe Expr)      
          | KeyWords       KeyWords Expr Expr           
          | Def            Expr Expr Expr
          |                Expr :->: Expr
          | TypeDef        Expr Expr

{-data Where a b   = Where a b
data LetIn a b   = LetIn a b
data CaseOf a b  = CaseOf a b-}

{-type IntegerList = List Integer
type FloatList = List Float
type DoubleList = List Double-}

{-data Value = ValInt Int 
          | ValFt   Float
          | ValDb   Double
          | ValCh   Char
          | ValBool Bool
          | ValL    (List Value)
          | ValEi   (Either Value Value)
          | ValMb   (Maybe Value)
          | ValPr   (Pair Value Value)-}

{-data List a = Nil | Cons a (List a)
data Pair a b = Pair a b-}
