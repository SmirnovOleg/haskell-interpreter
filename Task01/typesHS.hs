data List a = Nil | Cons a (List a)
data Pair a b = Pair a b

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
          | Double
          | Bool
          | Char

data Expr = NullLiteral
          | NameLiteral    String
          | IntLiteral     Int
          | DoubleLiteral  Double
          | CharLiteral    Char
          | StringLiteral  String
          | BoolLiteral    Bool
          | ListExpr       (List Expr)
          | PairExpr       (Pair Expr Expr)
          | Add            Expr Expr
          | Negate         Expr
          | Mul            Expr Expr
          | Frac           Expr Expr
          | Lambda         Expr
          | Ord            Expr OrdOp Expr
          | Logic          Expr LogicOp Expr   
          | IfThenClause   Expr Expr (Maybe Expr)      
          | KeyWords       KeyWords Expr Expr           
          | Def            Expr Expr Expr
          |                Expr :->: Expr

{-data Where a b   = Where a b
data LetIn a b   = LetIn a b
data CaseOf a b  = CaseOf a b-}

--data Func f a = Const a | Func (f a) a           -- Const Expr or Const ?????????? Also need to change
-- f x y ... :: type(x) -> (type(y) -> ...)

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

--data IfThenClause a b c = If a b (Maybe c)     -- if () then else 