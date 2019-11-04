module AST where

import qualified Data.Map as Map


type Name = String

data Type = Int
          | Bool
          | Char
        deriving (Show, Eq, Read)

data Pattern = NamePattern  Name
             | ListPattern  [Name]
             | PairPattern  (Name, Name)
        deriving (Show, Eq, Read)

data BinOp = Add | Sub | Mul | Div | 
             And | Or | Eq | Gt | Ls
        deriving (Show, Eq, Read)

data UnOp = Neg | Not 
        deriving (Show, Eq, Read)

data Expr = Ident          Name
          | App            Expr Expr
          | AppBinOp       BinOp Expr Expr
          | AppUnOp        UnOp Expr
          | Def            Name [Name] Expr  -- fix Name to Pattern 

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr]

          | Lambda         [Name] Expr Env   -- fix Name to Pattern

          | IntLiteral     Int
          | CharLiteral    Char
          | StringLiteral  String
          | BoolLiteral    Bool
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)

          |                Expr :->: Expr
          | TypeExpr       Type
          | TypeDef        Expr Expr         -- for expressions with ::
        deriving (Eq, Read) 


data HaskellError = BaseError String
                  | NotInScope Name
                  | WrongNumberOfArgs
                  | WrongType String
        deriving (Show, Eq, Read)

type Safe = Either HaskellError
type Env = Map.Map Name (Safe Expr)
