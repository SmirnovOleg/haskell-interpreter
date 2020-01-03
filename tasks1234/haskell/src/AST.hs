module AST where

import qualified Data.Map as Map
import Data.List (intercalate)


type Name = String

data Type = HInt
          | HBool
          | HChar
          | HList Type
          | HPair Type Type
          | HArrow Type Type
          | HVar Name
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

data UnOp = Neg | Not | Fst | Snd | Head | Tail
        deriving (Show, Ord, Eq, Read)

data Expr = Ident          Name
          | App            Expr Expr
          | Def            Name [Pattern] Expr 

          | AppBinOp       Expr BinOp Expr
          | AppUnOp        UnOp Expr
          | Let            Expr

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr]

          | Lambda         [Pattern] Expr Env

          | IntLiteral     Int
          | CharLiteral    Char
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
prettyPrint (ListExpr xs) = "[" ++ (intercalate "," (map prettyPrint xs)) ++ "]"
prettyPrint (PairExpr x) = "(" ++ (prettyPrint $ fst x) ++ "," ++ (prettyPrint $ snd x) ++ ")"
prettyPrint (Undefined) = "<undefined>"
prettyPrint (Lambda patterns body closure) = "<lambda>"
prettyPrint (Ident x) = show x
prettyPrint _ = ""

data HaskellError = BaseError String
                  | NotInScope Name
                  | WrongNumberOfArguments
                  | TypeError String
                  | ParseError
                  | UndefinedNameError
                  | WrongArgument
                  | NoInstanceForShow
                  | NotFoldable
                deriving (Show, Ord, Eq, Read)

type Safe = Either HaskellError
type Env = Map.Map Name [Safe Expr]
type TypedEnv = Map.Map Expr Type
