{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

module AST where

import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad.Except
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Eq (GEq, geq)


type Name = String

data Type = HInt
          | HBool
          | HChar
          | HList Type
          | HPair Type Type
          | HArrow Type Type
          | HVar Name
          | HUnknown
        deriving (Ord, Eq, Read, Generic)

data Pattern = NamePattern  Name
             | WildCardPattern
             | EmptyListPattern
             | ListPattern  Pattern Pattern
             | PairPattern  (Pattern, Pattern)
        deriving (Show, Ord, Eq, Read, Generic)

data BinOp = Add | Sub | Mul | Div | And | Or | Eq | Gt | Ls | Concat | Push
        deriving (Show, Ord, Eq, Read, Generic)

data UnOp = Neg | Not | Fst | Snd | Head | Tail
        deriving (Show, Ord, Eq, Read, Generic)

data Expr = Ident          Name
          | App            Expr Expr
          | Def            Name [Pattern] Expr 

          | AppBinOp       Expr BinOp Expr
          | AppUnOp        UnOp Expr
          | Let            Expr

          | IfThenElse     Expr Expr Expr
          | Where          Expr [Expr]

          | Lambda         [Pattern] Expr Env

          | IntLiteral     Integer
          | CharLiteral    Char
          | BoolLiteral    Bool
          | ListExpr       [Expr]
          | PairExpr       (Expr, Expr)
          
          | None
          | Undefined
        deriving (Show, Read, Generic)

data HaskellError = BaseError String
                  | NotInScope Name
                  | WrongNumberOfArguments
                  | TypeError String
                  | ParseError
                  | UndefinedNameError
                  | WrongArgument
                  | NoInstanceForShow
                  | TypeInferenceError
                deriving (Show, Ord, Eq, Read, Generic)

type Safe = Either HaskellError
type SafeT m a = ExceptT HaskellError m a
type Env = Map.Map Name [Safe (Expr, Type)]
type TEnv = [(Expr, Type)]
type Tvs = [Name]

instance GEq UnOp
instance GEq BinOp
instance GEq Pattern
instance GEq (Map.Map Name [Safe (Expr, Type)]) where
    geq _ _ = False
instance GEq Expr
instance Eq Expr where
    (==) (App f x) (App f' x') = 
        f == f' && x == x'
    (==) (IfThenElse p t e) (IfThenElse p' t' e') =
        p == p' && t == t' && e == e'
    (==) (PairExpr (x,y)) (PairExpr (x', y')) = 
        x == x' && y == y'
    (==) (ListExpr xs) (ListExpr xs') =
        all (\(x,y) -> x == y) (zip xs xs')
    (==) (Lambda ps b _) (Lambda ps' b' _) = 
        ps == ps' && b == b'
    (==) a b = geq a b

prettyPrint :: Expr -> String
prettyPrint (IntLiteral x) = show x
prettyPrint (BoolLiteral x) = show x
prettyPrint (CharLiteral x) = show x
prettyPrint (ListExpr xs) = "[" ++ (intercalate "," (map prettyPrint xs)) ++ "]"
prettyPrint (PairExpr x) = "(" ++ (prettyPrint $ fst x) ++ "," ++ (prettyPrint $ snd x) ++ ")"
prettyPrint Undefined = "<undefined>"
prettyPrint (Lambda patterns body closure) = "<lambda>"
prettyPrint (Ident x) = show x
prettyPrint (App func arg) = "(" ++ prettyPrint func ++ ") (" ++ prettyPrint arg ++ ")" 
prettyPrint _ = "?"

instance Show Type where
    show HInt = "Int"
    show HBool = "Bool"
    show HChar = "Char"
    show (HList a) = "[" ++ show a ++ "]"
    show (HPair a b) = "(" ++ show a ++ "," ++ show b ++ ")"
    show (HArrow a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (HVar a) = a
    show HUnknown = "?"