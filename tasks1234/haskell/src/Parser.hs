{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell#-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text, unpack)
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple 
import AST
import Runtime (runtime)
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
  return $ (ListExpr $ map (\c -> CharLiteral c) str)

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

binApplication = \op -> \x y -> App (App op x) y

binaryOp = makeOpParser binApplication

binaryL = binaryOp InfixL
binaryN = binaryOp InfixN
binaryR = binaryOp InfixR

prefixOp = makeOpParser App

prefix  = prefixOp Prefix
postfix = prefixOp Postfix
-------------------------------

-------------------------------
listBinOps = symbol <$> ["*", "concat", "-", "+", ":", ">", "<", "==", "&&", "||"]

operatorParser :: Parser Expr
operatorParser = do
  op <- choice (try <$> listBinOps)
  return $ Ident op

operationsTable :: [[Operator Parser Expr]]
operationsTable = 	[ [ prefix "-" ] 
	  				, [ binaryL "*", InfixL (symbol "`div`" >> return (binApplication (Ident "div"))) ]
					, [ binaryL "+", binaryL "-" ]
					, [ binaryL "concat", binaryR ":" ]
					, [ binaryN ">", binaryN "<", binaryN "==" ]
					, [ binaryR "&&" ] 
					, [ binaryR "||" ] 
					]
-------------------------------

exprParser :: Parser Expr
exprParser = makeExprParser termParser operationsTable

termParser :: Parser Expr
termParser = space >> choice [ try pairParser
							 , try applicationParser
							 , try lambdaParser
							 , try ifParser
							 , try literalParser
							 , try listParser
							 , try $ parens exprParser
							 ]

ifParser :: Parser Expr
ifParser = do
	lexeme $ string "if"
	condition <- (lexeme $ exprParser) <?> "logic operation/operand"
	lexeme $ string "then"
	then' <- exprParser
	lexeme $ string "else"
	else' <- exprParser
	return $ IfThenElse condition then' else' 

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

atomicPatterns = [namePatternParser, emptyListPatternParser, wildCardPatternParser]

allPatterns = [try $ parens listPatternParser, pairPatternParser] ++ atomicPatterns

namePatternParser = do
	(Ident name) <- identParser
	return $ NamePattern name

listPatternParser = do
	head <- choice allPatterns
	symbol ":"
	tail <- choice $ (try listPatternParser) : atomicPatterns
	return $ ListPattern head tail

emptyListPatternParser = do
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
	identsH <- some patternsParser <?> "at least one argument"
	string "->"
	(Lambda identsT body _) <- try (space1 >> lambdaParser) <|> do
		space
		body <- exprParser
		return $ Lambda [] body runtime
	return $ Lambda (identsH ++ identsT) body runtime

applicationParser :: Parser Expr
applicationParser = do
	func <- choice [ identParser, parens funcNameParser ]
	args <- many $ argumentParser
	notFollowedBy (do 				-- to be sure it is not declaration of the function
		eq <- char '=' 
		notEq <- satisfy (/= '=') 
		return $ eq : [notEq]) <?> "=="
	return $ foldl App func args

-- Implement CPS: operatorParser should be nested in applicationParser with parens !
funcNameParser :: Parser Expr
funcNameParser = choice [ applicationParser
						, operatorParser
						, lambdaParser ]

argumentParser :: Parser Expr
argumentParser = choice [ try identParser
						, try literalParser
						, try listParser
						, try pairParser
						, try $ parens ifParser
						, try $ parens exprParser
						, try $ parens applicationParser
						, try $ parens lambdaParser
						]

programParser :: Parser [Expr]
programParser = do
  sepEndBy1 (choice $ try <$> [whereParser , defParser]) eol

replParser :: Parser Expr
replParser = do
  space
  choice [try whereParser, try defParser, try letParser, exprParser]

letParser :: Parser Expr
letParser = do
	string "let"
	stmt <- choice [try whereParser, try defParser]
	return $ Let stmt

treeParser :: String -> IO ()
treeParser str = do
	case (runParser replParser "" str) of
		Right text -> pPrint text
		Left error -> putStrLn $ show error --parseTest replParser str
