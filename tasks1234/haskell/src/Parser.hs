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
keywords = [
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
  bool' <- lexeme (string "True" <|> string "False")
  return $ BoolLiteral $ (read bool' :: Bool)

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
  if (word `elem` keywords)
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
numOperatorsTable :: [[Operator Parser Expr]]
numOperatorsTable = operatorTableUn ++ operatorTableBin where
  operatorTableUn  = [ [ (prefix "-" (AppUnOp Neg)) , (prefix "+" id)] ]
  operatorTableBin = [ [ (binaryL "*" (AppBinOp Mul)) , (binaryL "`div`"(AppBinOp Div)) ]
                     , [ (binaryL "+" (AppBinOp Add)) , (binaryL "-" (AppBinOp Sub)) ]
                     ]

logicOperatorsTable :: [[Operator Parser Expr]] 
logicOperatorsTable = [ [ prefix "not" (AppUnOp Not) ]
                      , [ binaryR "&&" (AppBinOp And) ] 
                      , [ binaryR "||" (AppBinOp Or) ]
                      ]

listOperatorsTable :: [[Operator Parser Expr]]
listOperatorsTable = [ [ binaryR "++" (AppBinOp Concat), binaryR ":" (AppBinOp Push) ] ]

pairOperatorsTable :: [[Operator Parser Expr]]
pairOperatorsTable = [ [ prefix "fst" (AppUnOp Fst), prefix "snd" (AppUnOp Snd) ] ]
-------------------------------

-------------------------------
termsNumParser :: Parser Expr
termsNumParser = choice [parens numOperationsParser, applicationParser, identParser, numberParser]

termsLogicParser :: Parser Expr
termsLogicParser = choice [try $ parens logicOperationsParser, boolParser, orderOperationsParser]

termsListParser :: Parser Expr
termsListParser = choice [try $ parens listOperationsParser, stringParser, charParser
						 , listParser, pairParser, try logicOperationsParser, numOperationsParser]
-------------------------------

-------------------------------
numOperationsParser :: Parser Expr
numOperationsParser = makeExprParser termsNumParser numOperatorsTable

logicOperationsParser :: Parser Expr
logicOperationsParser = makeExprParser termsLogicParser logicOperatorsTable

orderOperationsParser :: Parser Expr
orderOperationsParser = (try $ parens orderOperationsParser) <|> do
							l <- numOperationsParser
							sgn <- (Eq <$ symbol "==") <|> (Ls <$ symbol "<") <|> (Gt <$ symbol ">")
							r <- numOperationsParser
							return $ AppBinOp sgn l r

listOperationsParser :: Parser Expr
listOperationsParser = makeExprParser termsListParser listOperatorsTable
-------------------------------

exprParser :: Parser Expr
exprParser = choice [ifParser, listOperationsParser, undefinedParser]

ifParser :: Parser Expr
ifParser = do
  lexeme $ string "if"
  cond <- lexeme logicOperationsParser
  lexeme $ string "then"
  then' <- choice [parens exprParser, exprParser]
  lexeme $ string "else"
  else' <- choice [parens exprParser, exprParser]
  return $ IfThenElse cond then' else' 

-------------------------------
inlineWhereBlockParser :: Parser [Expr]
inlineWhereBlockParser = do
	lexeme $ string "where" 
	defs <- between (symbol "{") (symbol "}") (sepBy1 defParser (symbol ";"))
			<|> (:[]) <$> defParser
	return defs

newLineWhereBlockParser :: Parser [Expr]
newLineWhereBlockParser = choice [
	try $ do
	  lexeme $ string "where" 
	  newline 
	  tabs <- some tab
	  defs <- sepEndBy1 defParser (eol >> string tabs)
	  return defs
  , do 
	  newline
	  spaceW <- some tab
	  lexeme $ string "where" 
	  newline
	  tabs <- (string spaceW >> some tab)
	  defs <- sepEndBy1 defParser (eol >> string tabs)
	  return defs
	   ]

whereParser :: Parser Expr
whereParser = do 
  stmt <- defParser
  defs <- newLineWhereBlockParser <|> inlineWhereBlockParser
  return $ Where stmt defs
-------------------------------

-------------------------------
underscorePatternParser = symbol "_" >> return UnderscorePattern

namePatternParser = do
  (Ident name) <- identParser
  return $ NamePattern name

listPatternParser = parens $ do
  hd <- choice [try listPatternParser, pairPatternParser, namePatternParser, emptyListParser, underscorePatternParser]
  symbol ":"
  tail <- choice [listPatternParser, namePatternParser, emptyListParser, underscorePatternParser]
  return $ ListPattern hd tail

emptyListParser = do
  between (symbol "[") (symbol "]") space
  return $ ListPattern EmptyListPattern EmptyListPattern

pairPatternParser = parens $ do
  fst <- choice [try listPatternParser, pairPatternParser, namePatternParser]
  symbol ","
  snd <- choice [try listPatternParser, pairPatternParser, namePatternParser]
  return $ PairPattern (fst, snd)
-------------------------------
  
defParser :: Parser Expr
defParser = do
  space
  (Ident funcN) <- lexeme identParser
  params <- many $ choice [try listPatternParser, pairPatternParser, namePatternParser, emptyListParser, underscorePatternParser]
  symbol "="
  stmt <- exprParser
  return $ Def funcN params stmt

applicationParser :: Parser Expr
applicationParser = do
  appFunc <- identParser
  param <- many $ choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedParser]
  return $ foldl App appFunc param

programParser :: Parser [Expr]
programParser = do
  sepEndBy1 (choice $ try <$> [whereParser , defParser]) eol

replParser :: Parser Expr
replParser = do
  choice $ try <$> [whereParser, defParser, exprParser]