module AST where

import qualified Data.Map as Map


type Name = String

data Type = Int
          | Bool
          | Char
        deriving (Show, Eq)

data Pattern = NamePattern  Name
             | ListPattern  [Name]
             | PairPattern  (Name, Name)
        deriving (Show, Eq)

data BinOp = Add | (:-:) | (:*:) | (:/:) | 
            (:&&:) | (:||:) | (:==:) | (:>:) | (:<:)
        deriving (Show, Eq)

data UnOp = Neg | Not 
        deriving (Show, Eq)

data Expr = Ident          Name
          | App            Expr [Expr]
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
        deriving (Show, Eq) 

data HaskellError = BaseError String
                  | NotInScope Name
                  | WrongNumberOfArgs
                  | WrongType String
        deriving (Show, Eq)

type Safe = Either HaskellError

type Env = Map.Map Name (Safe Expr)