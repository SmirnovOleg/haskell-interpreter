module AST where

import qualified Data.Map as Map

type Name = String

data Type = HInt
          | HBool
          | HChar
          | HList Type
          | HPair Type Type
          | HLambda Type Type
        deriving (Show, Ord, Eq, Read) 

data Pattern = NamePattern  Name
             | WildCardPattern
             | EmptyListPattern
             | ListPattern  Pattern Pattern
             | PairPattern  (Pattern, Pattern)
        deriving (Show, Ord, Eq, Read)

data BinOp = Add | Sub | Mul | Div 
           | And | Or | Eq | Gt | Ls 
           | Concat | Push
        deriving (Show, Ord, Eq, Read)

data UnOp = Neg | Not | Fst | Snd
        deriving (Show, Ord, Eq, Read)

data Expr = Ident          Name
          | App            Expr Expr
          | AppBin         Expr Expr Expr
          | AppBinOp       BinOp Expr Expr
          | AppUnOp        UnOp Expr
          | Def            Name [Pattern] Expr 

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr]

          | Lambda         [Pattern] Expr Env
          | UserLambda     [Pattern] Expr

          | IntLiteral     Int
          | CharLiteral    Char
          | StringLiteral  String
          | BoolLiteral    Bool
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)
          
          | None
          | Undefined
        deriving (Show, Ord, Eq, Read)

prettyPrint :: Expr -> String
prettyPrint (IntLiteral x) = show x
prettyPrint (BoolLiteral x) = show x
prettyPrint (CharLiteral x) = show x
prettyPrint (StringLiteral x) = show x
prettyPrint (ListExpr x) = show x
prettyPrint (PairExpr x) = show x
prettyPrint (Undefined) = "<undefined>"
prettyPrint (None) = ""
prettyPrint lambda@(Lambda patterns body closure) = "<lambda>"
prettyPrint (Ident x) = show x
prettyPrint _ = ""

data HaskellError = BaseError String
                  | NotInScope Name
                  | WrongNumberOfArguments
                  | TypeError String
                  | ParseError
                  | UndefinedNameError
                  | WrongArgument
                deriving (Show, Ord, Eq, Read)

type Safe = Either HaskellError
type Env = Map.Map Name (Safe Expr)
