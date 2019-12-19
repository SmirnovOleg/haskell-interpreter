{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell#-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple 
import AST
import Data.Function ((&))

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
keywords =  ["True", "False"
            , "where"
            , "let", "in"
            , "do"
            , "if", "then", "else"
            , "case", "of"
            , "undefined"
            ]
-------------------------------

-------------------------------
undefinedExprParser :: Parser Expr
undefinedExprParser = do
  lexeme $ string "Undefined"
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
  head <- lowerChar
  tail <- many $ choice [alphaNumChar, char '_', char '\'']
  let word = head : tail
  if (word `elem` keywords)
     then fail $ "Name " ++ word ++ " is keyword"
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
makeOpParser application operator name = 
	operator (symbol name >>= \n -> return $ application (Ident n))

binaryOp = makeOpParser AppBin

binaryL = binaryOp InfixL
binaryN = binaryOp InfixN
binaryR = binaryOp InfixR

prefixOp = makeOpParser App

prefix  = prefixOp Prefix
postfix = prefixOp Postfix
-------------------------------

-------------------------------
listBinOps = symbol <$> ["*", "++", "+", ":", ">", "<", "==", "&&", "||"]

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
numOperandsParser = choice  [ try applicationParser
							, try $ parens numOperationsParser
							, numberParser]

logicOperandsParser :: Parser Expr
logicOperandsParser = choice [ try $ parens logicOperationsParser
							 , boolParser
							 , orderOperationsParser]

orderOperandsParser :: Parser Expr
orderOperandsParser = choice [ parens numOperationsParser
							 , numOperationsParser]

listOperandsParser :: Parser Expr
listOperandsParser = choice [ try applicationParser
							, try $ parens logicOperationsParser
							, try $ parens numOperationsParser
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
exprParser = space >> choice [ try logicOperationsParser
							 , try listOperationsParser
							 , try orderOperationsParser
							 , try numOperationsParser
							 , ifParser
							 , try $ parens exprParser
							 , undefinedExprParser]

ifParser :: Parser Expr
ifParser = do
	lexeme $ string "if"
	cond <- (lexeme $ choice  [ try logicOperationsParser
								, try applicationParser
								, identParser]) <?> "logic operation/operand"
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

atomicPatterns = [namePatternParser, emptyListParser, wildCardPatternParser]

allPatterns = [try listPatternParser, pairPatternParser] ++ atomicPatterns

namePatternParser = do
	(Ident name) <- identParser
	return $ NamePattern name

listPatternParser = parens $ do
	hd <- choice allPatterns
	symbol ":"
	tail <- choice $ [listPatternParser] ++ atomicPatterns
	return $ ListPattern hd tail

emptyListParser = do
	between (symbol "[") (symbol "]") space
	return $ EmptyListPattern

pairPatternParser = parens $ do
	fst <- choice allPatterns
	symbol ","
	snd <- choice allPatterns
	return $ PairPattern (fst, snd)

patternsParser :: Parser Pattern
patternsParser = choice allPatterns <?> "argument"

-------------------------------

defParser :: Parser Expr
defParser = do
	space
	(Ident funcN) <- lexeme identParser <?> "function name"
	params <- many patternsParser
	symbol "="
	expr <- exprParser <|> lambdaParser
	return $ Def funcN params expr

lambdaParser :: Parser Expr
lambdaParser = do
	symbol "\\"
	identsH <- some patternsParser <?> "at least one arguement"
	string "->"
	(UserLambda identsT body) <- try (space1 >> lambdaParser) <|> do
		space
		body <- exprParser
		return $ UserLambda [] body
	return $ UserLambda (identsH ++ identsT) body

binOpAsFuncParser :: Parser Expr
binOpAsFuncParser = do
	sgn <- parens (choice $ try <$> listBinOps)
	l <- choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedExprParser]
	r <- choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedExprParser]
	return $ App (App (Ident sgn) l) r

partAppBinOpParser :: Parser Expr
partAppBinOpParser = do
	(sgn, l) <- parens (do
		sgn <- choice $ try <$> listBinOps
		first <- choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedExprParser]
		return (sgn, first)
		)
	r <- choice [parens exprParser, identParser, literalParser, listParser, pairParser, undefinedExprParser]
	return $ App (App (Ident sgn) l) r

lambdaApplicationParser :: Parser Expr
lambdaApplicationParser = do
	lambda@(UserLambda idents body) <- parens lambdaParser <?> "lambda application should be in parens"
	args <- many appArgs
	if ((idents & length) > (args & length))
		then fail "not enough arguments for lambda function"
		else return $ foldl App lambda args

applicationParser :: Parser Expr
applicationParser = 
	choice [ try partAppBinOpParser
		   , try binOpAsFuncParser
		   , try lambdaApplicationParser
		   , do
				appFunc <- choice [try $ parens exprParser, identParser]
				args <- many $ choice [ try $ parens exprParser
									  , try $ parens lambdaParser
									  , identParser
									  , literalParser
									  , listParser
									  , pairParser
									  , undefinedExprParser]
				notFollowedBy (do -- to be sure it is not declaration of the function
					eq <- char '=' 
					notEq <- satisfy (/= '=') 
					return $ eq : [notEq]) <?> "=="
				return $ foldl App appFunc args]

appArgs :: Parser Expr
appArgs = choice [ parens exprParser
				 , identParser
				 , literalParser
				 , listParser
				 , pairParser
				 , undefinedExprParser]

programParser :: Parser [Expr]
programParser = do
  sepEndBy1 (choice $ try <$> [whereParser , defParser]) eol

replParser :: Parser Expr
replParser = do
  space
  choice [try whereParser, try defParser, exprParser]

treeParser :: String -> IO ()
treeParser str = do
	case (runParser replParser "" str) of
		Right text -> pPrint text
		Left error -> putStrLn $ show error --parseTest replParser str
