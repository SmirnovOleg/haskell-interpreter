data List a = Nil | Cons a (List a)
data Pair a b = Pair a b

data Comparisons = Equ
                 | Lt
                 | Gt
                 | Neq

data KeyWords = Where
              | LetIn
              | CaseOf

-- Condition has next view: [Expr] so it's just ListExpr (Cons (Expr EqualOps Expr) (Cons (Expr EqualOps Expr) (...)))
-- Fun (Const "<NameFun>") (ListExpr (List (Const Char))) :=: Expr
data Expr = Const Char
          | ListExpr     (List Expr)
          | Add          Expr Expr
          | Mul          Expr Expr
          | Fun          Expr Expr                       -- Fun (Const "<NameFun>") ListParams
          | Lambda       Expr                            -- Lambda Expr :->: Expr
          | Param        Expr                            -- Param (Const "<Name>")
          | CompareExpr  Expr Comparisons Expr           -- i.e. CompareExpr Expr Neq Expr   
          | IfThenClause Expr Expr (Maybe Expr)      
          | KWords       KeyWords Expr Expr           
          | Expr :=: Expr                                -- Def or :=:  - what should we choose
          | Expr :->: Expr                               -- special for case Expr of Expr -> Expr



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