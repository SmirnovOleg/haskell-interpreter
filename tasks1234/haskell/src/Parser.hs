{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell#-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple 
import AST
import Tests

type Text = String

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
keywords =  [
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
  (char '\"') 
  str <- manyTill anySingle (symbol "\"")
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
makeOpParser f operator name = operator (symbol name >>= \n -> return $ f (Ident name))

binaryOp = makeOpParser AppBin

binaryL = binaryOp InfixL
binaryN = binaryOp InfixN
binaryR = binaryOp InfixR

prefixOp = makeOpParser App

prefix  = prefixOp Prefix
postfix = prefixOp Postfix
-------------------------------

-------------------------------
operationsTable :: [[Operator Parser Expr]]
operationsTable = 	[ [ prefix "-" ] 
	  				, [ binaryL "*", InfixL (symbol "`div`" >> return (AppBin (Ident "div"))) ]
					, [ binaryL "+", binaryL "-" ]
					, [ binaryL "++", binaryR ":" ]
					, [ binaryN ">", binaryN "<", binaryN "==" ]
					]

logicOperationsTable :: [[Operator Parser Expr]]
logicOperationsTable =  [ [ prefix "not" ]
						, [ binaryR "&&" ] 
						, [ binaryR "||" ] 
						]
-------------------------------

-------------------------------
numOperandsParser :: Parser Expr
numOperandsParser = choice  [ try applicationParser, try $ parens numOperationsParser
							, numberParser]

logicOperandsParser :: Parser Expr
logicOperandsParser = choice [try $ parens logicOperationsParser, boolParser, orderOperationsParser]

orderOperandsParser :: Parser Expr
orderOperandsParser = parens numOperationsParser <|> numOperationsParser

listOperandsParser :: Parser Expr
listOperandsParser = choice [ try applicationParser, try $ parens logicOperationsParser, try $ parens numOperationsParser
						 	, parens listOperationsParser, pairParser, literalParser
						 	, listParser]
-------------------------------

-------------------------------
numOperationsParser :: Parser Expr
numOperationsParser = makeExprParser numOperandsParser operationsTable

logicOperationsParser :: Parser Expr
logicOperationsParser = makeExprParser logicOperandsParser logicOperationsTable

orderOperationsParser :: Parser Expr
orderOperationsParser = makeExprParser orderOperandsParser operationsTable

listOperationsParser :: Parser Expr
listOperationsParser = makeExprParser listOperandsParser operationsTable
-------------------------------

exprParser :: Parser Expr
exprParser = space >> choice [ try numOperationsParser
							 , try logicOperationsParser, try listOperationsParser
							 , try orderOperationsParser
							 , ifParser, try $ parens exprParser
							 , undefinedParser]

ifParser :: Parser Expr
ifParser = do
  lexeme $ string "if"
  cond <- (lexeme $ choice  [ try logicOperationsParser
  							, try applicationParser, identParser]) <?> "logic operation/operand"
  lexeme $ string "then"
  then' <- choice [parens exprParser, exprParser]
  lexeme $ string "else"
  else' <- choice [parens exprParser, exprParser]
  return $ IfThenElse cond then' else' 

-------------------------------
inlineWhereBlockParser :: Parser [Expr]
inlineWhereBlockParser = do
	lexeme $ string "where" 
	defs <- between (symbol "{") (symbol "}") (sepBy1 (choice [try whereParser, defParser]) (symbol ";"))
			<|> (:[]) <$> (choice [try whereParser, defParser])
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
wildCardPatternParser = symbol "_" >> return WildCardPattern

nameEmptyWildCard = [namePatternParser, emptyListParser, wildCardPatternParser]

namePatternParser = do
  (Ident name) <- identParser
  return $ NamePattern name

listPatternParser = parens $ do
  hd <- choice $ [try listPatternParser, pairPatternParser] ++ nameEmptyWildCard
  symbol ":"
  tail <- choice $ [listPatternParser] ++ nameEmptyWildCard
  return $ ListPattern hd tail

emptyListParser = do
  between (symbol "[") (symbol "]") space
  return $ EmptyListPattern

pairPatternParser = parens $ do
  fst <- choice $ [try listPatternParser, pairPatternParser] ++ nameEmptyWildCard
  symbol ","
  snd <- choice $ [try listPatternParser, pairPatternParser] ++ nameEmptyWildCard
  return $ PairPattern (fst, snd)
-------------------------------
  
defParser :: Parser Expr
defParser = do
  space
  (Ident funcN) <- lexeme identParser <?> "function name"
  params <- many (choice [try listPatternParser, pairPatternParser
  						  , namePatternParser, emptyListParser, wildCardPatternParser] <?> "argument")
  symbol "="
  stmt <- exprParser
  return $ Def funcN params stmt

applicationParser :: Parser Expr
applicationParser = do
  appFunc <- choice [try $ parens exprParser, identParser]
  param <- many $ choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedParser]
  notFollowedBy (do 
	eq <- char '=' 
	notEq <- satisfy (/= '=') 
	return $ eq : [notEq]) <?> "=="
  return $ foldl App appFunc param

programParser :: Parser [Expr]
programParser = do
  sepEndBy1 (choice $ try <$> [whereParser , defParser]) eol

replParser :: Parser Expr
replParser = do
  space
  choice [try whereParser, try defParser, exprParser]

treeParser :: String -> IO ()
treeParser str = do
	case (parseMaybe replParser str) of
		Just text -> pPrint text
		Nothing -> parseTest replParser str