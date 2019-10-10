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
          | Mul            Expr Expr
          | Frac           Expr Expr
          | Lambda         Expr
          | Func           Expr Expr
          | Ord            Expr OrdOp Expr
          | Logic          Expr LogicOp Expr   
          | IfThenClause   Expr Expr (Maybe Expr)      
          | KeyWords       KeyWords Expr Expr           
          | Def            Expr Expr
          |                Expr :->: Expr
          | TypeDef        Expr Expr
          deriving Show

factorial :: Expr          
factorial = Def (Func (NameLiteral "factorial") (ListExpr [NameLiteral "n"])) (IfThenClause (Ord (NameLiteral "n") Eq (IntLiteral 0)) (IntLiteral 1) (Just (Mul (IntLiteral 0) (Func (NameLiteral "factorial") (ListExpr [NameLiteral ("n-1")])))))

{-Few demo simples
factorial


Def (NameLiteral "factorial") [NameLiteral n] (IfThenClause (Ord (NameLiteral n) Eq (NumLiteral 0)) (NumLiteral 1) (Just (Mul (NameLiteral 0) (NameLiteral factorial [NameLiteral (n-1)]))))

-}

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
