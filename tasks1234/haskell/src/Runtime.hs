module Runtime where

import AST
import qualified Data.Map as Map


binaryOp :: BinOp -> Expr
binaryOp op = Lambda [NamePattern "x", NamePattern "y"] (AppBinOp (Ident "x") op (Ident "y")) Map.empty

unaryOp :: UnOp -> Expr
unaryOp op = Lambda [NamePattern "x"] (AppUnOp op (Ident "x")) Map.empty

runtime :: Env
runtime = Map.fromList [ ("+", [return (binaryOp Add, HArrow HInt (HArrow HInt HInt))])
                       , ("-", [return (binaryOp Sub, HArrow HInt (HArrow HInt HInt))])
                       , ("*", [return (binaryOp Mul, HArrow HInt (HArrow HInt HInt))])
                       , ("`div`", [return (binaryOp Div, HArrow HInt (HArrow HInt HInt))])
                       , ("&&", [return (binaryOp And, HArrow HBool (HArrow HBool HBool))])
                       , ("||", [return (binaryOp Or, HArrow HBool (HArrow HBool HBool))])
                       , ("==", [return (binaryOp Eq, HArrow (HVar "a") (HArrow (HVar "a") HBool))])
                       , (">", [return (binaryOp Gt, HArrow (HVar "a") (HArrow (HVar "a") HBool))])
                       , ("<", [return (binaryOp Ls, HArrow (HVar "a") (HArrow (HVar "a") HBool))])
                       , ("concat", [return (binaryOp Concat, HArrow (HList (HVar "a")) (HArrow (HList (HVar "a")) (HList (HVar "a"))))])
                       , (":", [return (binaryOp Push, HArrow (HVar "a") (HArrow (HList (HVar "a")) (HList (HVar "a"))))])
                       , ("neg", [return (unaryOp Neg, HArrow HInt HInt)])
                       , ("not", [return (unaryOp Not, HArrow HBool HBool)])
                       , ("fst", [return (unaryOp Fst, HArrow (HPair (HVar "a") (HVar "b")) (HVar "a"))])
                       , ("snd", [return (unaryOp Snd, HArrow (HPair (HVar "a") (HVar "b")) (HVar "b"))])
                       , ("head", [return (unaryOp Head, HArrow (HList (HVar "a")) (HVar "a"))])
                       , ("tail", [return (unaryOp Tail, HArrow (HList (HVar "a")) (HList (HVar "a")))]) ]

tvs :: [Name]
tvs = map (\x -> init $ tail $ show x) "cdefghijklmnopqrstuvwxyz"

calcBinOp :: Expr -> BinOp -> Expr -> Safe Expr
calcBinOp (IntLiteral a) Add (IntLiteral b) = return $ IntLiteral (a + b)
calcBinOp _ Add _  = Left $ TypeError "Int in + operator expected"
calcBinOp (IntLiteral a) Sub (IntLiteral b) = return $ IntLiteral (a - b)
calcBinOp _ Sub _  = Left $ TypeError "Int in - operator expected"
calcBinOp (IntLiteral a) Mul (IntLiteral b) = return $ IntLiteral (a * b)
calcBinOp _ Mul _  = Left $ TypeError "Int in * operator expected"
calcBinOp (IntLiteral a) Div (IntLiteral b) = return $ IntLiteral (a `div` b)
calcBinOp _ Div _  = Left $ TypeError "Int in `div` operator expected"

calcBinOp (BoolLiteral a) And (BoolLiteral b) = return $ BoolLiteral (a && b)
calcBinOp _ And _  = Left $ TypeError "Bool in && operator expected"
calcBinOp (BoolLiteral a) Or (BoolLiteral b) = return $ BoolLiteral (a || b)
calcBinOp _ Or _  = Left $ TypeError "Bool in || operator expected"

calcBinOp (IntLiteral a) Eq (IntLiteral b) = return $ BoolLiteral (a == b)
calcBinOp (BoolLiteral a) Eq (BoolLiteral b) = return $ BoolLiteral (a == b)
calcBinOp _ Eq _  = Left $ TypeError "Int or Bool in == operator expected"
calcBinOp (IntLiteral a) Ls (IntLiteral b) = return $ BoolLiteral (a < b)
calcBinOp _ Ls _  = Left $ TypeError "Int in < operator expected"
calcBinOp (IntLiteral a) Gt (IntLiteral b) = return $ BoolLiteral (a > b)
calcBinOp _ Gt _  = Left $ TypeError "Int in > operator expected"

calcBinOp (ListExpr xs) Concat (ListExpr ys) = return $ ListExpr (xs ++ ys)
calcBinOp _ Concat _  = Left $ TypeError "Lists in `concat` operator expected"
calcBinOp (x) Push (ListExpr xs) = return $ ListExpr (x : xs)
calcBinOp _ Push _  = Left $ TypeError "Expr and [Expr] in : operator expected"


calcUnOp :: UnOp -> Expr -> Safe Expr
calcUnOp Neg (IntLiteral a) = return $ IntLiteral (-a)
calcUnOp Neg _  = Left $ TypeError "Int in unary - operator expected"

calcUnOp Not (BoolLiteral a) = return $ BoolLiteral (not a)
calcUnOp Not _  = Left $ TypeError "Bool in not operator expected"

calcUnOp Fst (PairExpr (p1, _)) = return $ p1
calcUnOp Fst _  = Left $ TypeError "Pair in `fst` function expected"
calcUnOp Snd (PairExpr (_, p2)) = return $ p2
calcUnOp Snd _  = Left $ TypeError "Pair in `snd` function expected"

calcUnOp Head (ListExpr (head:tail)) = return $ head
calcUnOp Head _  = Left $ TypeError "List in `head` function expected"
calcUnOp Tail (ListExpr (head:tail)) = return $ ListExpr tail
calcUnOp Tail _  = Left $ TypeError "List in `tail` function expected"