{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parser where

--import           NeatInterpolation              ( text )
import Control.Monad                  ( void )
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import AST

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lineComment  = L.skipLineComment "--"
blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved = [
            "true",
            "false",
            "where",
            "let",
            "in",
            "do",
            "if",
            "then",
            "else",
            "case",
            "of"
           ]

operations = ["+", "-", "/", "*"]

boolParser :: Parser Expr
boolParser = 
  (trueParser >> return (BoolLiteral True)) <|> 
  (falseParser >> return (BoolLiteral False))
    where
      trueParser = string' "true"
      falseParser = string' "false"

{-numberParser :: Parser Expr
numberParser = (negativeParser <|> integerParser) >>= (\x -> return $ (IntLiteral . read) $ x)
        where
            negativeParser :: Parser String
            negativeParser = do
                neg <- char '-'
                int <- integerParser
                return (neg:int)
            
            integerParser :: Parser String
            integerParser = some digitChar
-}

numberParser :: Parser Expr
numberParser = do
  num <- lexeme L.decimal
  return $ IntLiteral num

charParser :: Parser Expr
charParser = do
    char '\''
    ch <- anySingle
    char '\''
    return (CharLiteral ch)

stringParser :: Parser Expr
stringParser = between (char '\"') (char '\"') (many anySingle) >>= (\x -> return $ StringLiteral . show $ x)

literalParser = choice [numberParser, charParser, stringParser, boolParser]

identParser :: Parser Expr
{-identParser = Ident <$> (lexeme $ do
                  begin <- lowerChar <?> "begin of name in alphabetic case"
                  end <- many alphaNumChar
                  let word = begin:end
                  if (word `elem` reserved)
                    then fail $ "name " ++ word ++ " is reserved"
                    else return word)-}
identParser = (lexeme . try) $ do
  begin <- letterChar
  end <- many alphaNumChar
  let word = begin : end
  if (word `elem` reserved)
     then fail $ "name " ++ word ++ " is reserved"
     else return $ Ident word

{-listParser :: Parser Expr
listParser =  -}

getNameFromIdentList :: [Expr] -> [Name]
getNameFromIdentList []             = []
getNameFromIdentList (Ident x : xs) = x : getNameFromIdentList xs

parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")

makeOpParser operator name f = operator (f <$ symbol name)

binaryL, binaryN, binaryR :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryL = makeOpParser InfixL
binaryN = makeOpParser InfixN
binaryR = makeOpParser InfixR
  
prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  = makeOpParser Prefix
postfix = makeOpParser Postfix

termsParser :: Parser Expr -> Parser Expr
termsParser parser = choice
  [ parens parser
  , applicationParser
  , identParser
  , numberParser ]

operatorTable :: [[Operator Parser Expr]]
operatorTable = operatorTableUn ++ operatorTableBin where
  operatorTableUn  = [ [ (prefix "-" (AppUnOp Neg)) , (prefix "+" id)] ]
  operatorTableBin = [ [ (binaryL "*" (AppBinOp Mul)) , (binaryL "/"(AppBinOp Div)) ]
                     , [ (binaryL "+" (AppBinOp Add)) , (binaryL "-" (AppBinOp Sub)) ]
                     ]

opsParser = termsParser operatorsParser

operatorsParser :: Parser Expr
operatorsParser = makeExprParser opsParser operatorTable

boolExprsTable = [ [ prefix "not" (AppUnOp Not) ]
                 , [ binaryN "==" (AppBinOp Eq)
                   , binaryN "<" (AppBinOp Lt) , binaryN ">" (AppBinOp Gt) ]
                 , [ binaryR "&&" (AppBinOp And) ] 
                 , [ binaryR "||" (AppBinOp Or) ]
                 ]

logicSeqParser :: Parser Expr
logicSeqParser = termsParser boolExprsParser
                 
boolExprsParser :: Parser Expr
boolExprsParser = makeExprParser logicSeqParser boolExprsTable

statement :: Parser Expr
statement = choice [ifParser, operatorsParser]

ifParser :: Parser Expr
ifParser = do
  lexeme $ string "if"
  cond <- lexeme boolExprsParser
  lexeme $ string "then"
  then' <- lexeme statement
  lexeme $ string "else"
  else' <- lexeme statement
  return $ IfThenElse cond then' else' 

{-whereParser :: Parser Expr
whereParser = do 
  stmt <- statement
  choice [lexeme $ string "where" >> newline, newline >> L.lexeme scn $ string "where"]-}





  {-funcAppParser :: Parser Expr
funcAppParser = choice [ do
                  f <- identParser 
                  between (some space) (some space) (char '.')
                  return $ -}

applicationParser :: Parser Expr
{-applicationParser = do
  appFunc <- identParser
  params <- manyTill (choice [identParser, literalParser]) (statement)
  return $ App appFunc params-}
applicationParser = do
  appFunc <- identParser
  param <- many $ choice [parens statement, identParser, literalParser]
  return $ foldl App appFunc param
  
  {-helper appFunc where
    helper func = do
      param <- many $ choice [parens statement, identParser, literalParser]
      return $ App appFunc param-}



defParser :: Parser Expr
defParser = do
  skipMany $ symbol " "
  (Ident funcN) <- lexeme identParser
  params <- (lexeme $ some identParser) <|> (Parser $ Ident [])
  lexeme $ symbol "="
  stmt <- statement
  if (params == [])
    then
    return $ Def funcN stmt
    else return $ Def funcN (NamePattern <$> (getNameFromIdentList params)) stmt


parseLine :: Parser String
parseLine = lexeme $ some (notFollowedBy eol >> anySingle)

parseComplexLine :: Parser (String, [String])
parseComplexLine = L.indentBlock scn p
  where
    p = do
      header <- parseLine
      return (L.IndentMany Nothing (return . (header, )) parseLineFold)

parseLineFold :: Parser String
parseLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
  in unwords <$> ps <* scn

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pComplexItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

myfunc a b = a + b * d + g
  where
        d = 5
        g = 4

parseTwoLines :: Parser String
parseTwoLines = do
  x <- L.lexeme scn $ some alphaNumChar
  --newline
  y <- lexeme $ some alphaNumChar
  return $ x ++ y