{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AST

type Parser = Parsec Void String

-------------------------------
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lineComment  = L.skipLineComment "--"
blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
-------------------------------

-------------------------------
keyword = [
             "True", "False"
            , "where"
            , "let", "in"
            , "do"
            , "if", "then", "else"
            , "case", "of"
            , "undefined"
           ]
-------------------------------

-------------------------------
undefinedParser :: Parser Expr
undefinedParser = do
  lexeme $ string "undefined"
  return Undefined

boolParser :: Parser Expr
boolParser = do
  _bool <- lexeme (string "True" <|> string "False")
  return $ BoolLiteral $ (read _bool :: Bool)

numberParser :: Parser Expr
numberParser = do
  num <- lexeme L.decimal
  return $ IntLiteral num

charParser :: Parser Expr
charParser = do
  chr <- between (char '\'') (symbol "\'") anySingle 
  return $ CharLiteral chr

stringParser :: Parser Expr
stringParser = do
  str <- between (char '\"') (symbol "\"") (many anySingle) 
  return $ StringLiteral str

literalParser = choice [numberParser, charParser, stringParser, boolParser]

identParser :: Parser Expr
identParser = (lexeme . try) $ do
  hd <- lowerChar
  tl <- many $ choice [alphaNumChar, char '_', char '\'']
  let word = hd : tl
  if (word `elem` keyword)
     then fail $ "name " ++ word ++ " is keyword"
     else return $ Ident word

listParser :: Parser Expr
listParser = do
  list <- between (symbol "[") (symbol "]") $ sepBy exprParser $ symbol ","
  return $ ListExpr list

pairParser :: Parser Expr
pairParser = parens $ do
  left <- exprParser
  symbol ","
  right <- exprParser
  return $ PairExpr (left, right)
-------------------------------

-------------------------------
makeOpParser operator name f = operator (f <$ symbol name)

binaryL, binaryN, binaryR :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryL = makeOpParser InfixL
binaryN = makeOpParser InfixN
binaryR = makeOpParser InfixR
  
prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  = makeOpParser Prefix
postfix = makeOpParser Postfix
-------------------------------

-------------------------------
operatorTable :: [[Operator Parser Expr]]
operatorTable = operatorTableUn ++ operatorTableBin where
  operatorTableUn  = [ [ (prefix "-" (AppUnOp Neg)) , (prefix "+" id)] ]
  operatorTableBin = [ [ (binaryL "*" (AppBinOp Mul)) , (binaryL "/"(AppBinOp Div)) ]
                     , [ (binaryL "+" (AppBinOp Add)) , (binaryL "-" (AppBinOp Sub)) ]
                     ]

boolExprsTable :: [[Operator Parser Expr]]                     
boolExprsTable = [ [ prefix "not" (AppUnOp Not) ]
                 , [ binaryN "==" (AppBinOp Eq)
                   , binaryN "<" (AppBinOp Ls) , binaryN ">" (AppBinOp Gt) ]
                 , [ binaryR "&&" (AppBinOp And) ] 
                 , [ binaryR "||" (AppBinOp Or) ]
                 ]

listExprsTable :: [[Operator Parser Expr]]
listExprsTable = [ [prefix "fst" (AppUnOp Fst), prefix "snd" (AppUnOp Snd)]
                 , [ binaryL "++" (AppBinOp Concat), binaryR ":" (AppBinOp Push)]]
-------------------------------

-------------------------------
termsParsers :: Parser Expr -> [Parser Expr]
termsParsers parser = [ parens parser
                      , applicationParser
                      , identParser]

termsNumParser :: Parser Expr
termsNumParser = choice $ (termsParsers numExprsParser) ++ [numberParser]

termsLogicParser :: Parser Expr
termsLogicParser = choice $ (termsParsers boolExprsParser) ++ [numberParser, boolParser]

termsListParser :: Parser Expr
termsListParser = choice $ (termsParsers listExprsParser) ++ [stringParser, listParser]
-------------------------------

-------------------------------
numExprsParser :: Parser Expr
numExprsParser = makeExprParser termsNumParser operatorTable

boolExprsParser :: Parser Expr
boolExprsParser = makeExprParser termsLogicParser boolExprsTable

listExprsParser :: Parser Expr
listExprsParser = makeExprParser termsListParser listExprsTable
-------------------------------

exprParser :: Parser Expr
exprParser = choice [ifParser, numExprsParser, listExprsParser, undefinedParser, boolExprsParser]

ifParser :: Parser Expr
ifParser = do
  lexeme $ string "if"
  cond <- lexeme boolExprsParser
  lexeme $ string "then"
  then' <- choice [try $ lexeme exprParser, parens exprParser]
  lexeme $ string "else"
  else' <- choice [try $ lexeme exprParser, parens exprParser]
  return $ IfThenElse cond then' else' 

whereParser :: Parser Expr
whereParser = do 
  stmt <- defParser
  choice [
      try $ do
        lexeme $ string "where" 
        newline 
        tabs <- some tab
        defs <- sepEndBy1 defParser (eol >> string tabs)
        return $ Where stmt defs
    , do 
        newline
        spaceW <- some tab
        lexeme $ string "where" 
        newline
        tabs <- (string spaceW >> some tab)
        defs <- sepEndBy1 defParser (eol >> string tabs)
        return $ Where stmt defs
         ]

applicationParser :: Parser Expr
applicationParser = do
  appFunc <- identParser
  param <- many $ choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedParser]
  return $ foldl App appFunc param

parseAnyPattern = symbol "_" >> return AnyPattern

parseNamePattern = do
  (Ident name) <- identParser
  return $ NamePattern name

parseListPattern = parens $ do
  hd <- choice [try parseListPattern, parsePairPattern, parseNamePattern, parseEmptyList, parseAnyPattern]
  symbol ":"
  tail <- choice [parseListPattern, parseNamePattern, parseEmptyList, parseAnyPattern]
  return $ ListPattern hd tail

parseEmptyList = do
  between (symbol "[") (symbol "]") space
  return $ ListPattern EmptyListPattern EmptyListPattern

parsePairPattern = parens $ do
  fst <- choice [try parseListPattern, parsePairPattern, parseNamePattern]
  symbol ","
  snd <- choice [try parseListPattern, parsePairPattern, parseNamePattern]
  return $ PairPattern (fst, snd)
  
defParser :: Parser Expr
defParser = do
  space
  (Ident funcN) <- lexeme identParser
  params <- many $ choice [try parseListPattern, parsePairPattern, parseNamePattern, parseEmptyList, parseAnyPattern]
  symbol "="
  stmt <- exprParser
  return $ Def funcN params stmt

programParser :: Parser [Expr]
programParser = do
  sepEndBy1 (choice $ try <$> [whereParser , defParser , identParser]) eol

replParser :: Parser Expr
replParser = do
  choice $ try <$> [whereParser, defParser, exprParser]