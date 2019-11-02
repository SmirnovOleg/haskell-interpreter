module Parser where

--import           NeatInterpolation              ( text )
import Control.Monad                  ( void )
import Control.Monad.Combinators.Expr
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import AST

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "--"
  blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

boolParser :: Parser Expr
boolParser = 
  (trueParser >> return (BoolLiteral True)) <|> 
  (falseParser >> return (BoolLiteral False))
    where
      trueParser = string' "true"
      falseParser = string' "false"

numberParser :: Parser Expr
numberParser = (negativeParser <|> integerParser) >>= (\x -> return $ (IntLiteral . read) $ x)
        where
            negativeParser :: Parser String
            negativeParser = try $ do
                neg <- char '-'
                int <- integerParser
                return (neg:int)
            
            integerParser :: Parser String
            integerParser = try $ some digitChar


charParser :: Parser Expr
charParser = do
    char '\''
    ch <- anySingle
    char '\''
    return (CharLiteral ch)

stringParser :: Parser Expr
stringParser = between (char '\"') (char '\"') (many anySingle) >>= (\x -> return $ StringLiteral . show $ x)

literalParser = choice [numberParser, charParser, stringParser, boolParser]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ (prefix "-" Neg) , (prefix "+" id) ]
  , [ (binary "*" (:*:)) , (binary "/" (:/:)) ]
  , [ (binary "+" (:+:)) , (binary "-" (:-:)) ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)
  
prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

termsParser :: Parser Expr
termsParser = choice
  [ parens operatorsParser
  , literalParser ]

operatorsParser :: Parser Expr
operatorsParser = makeExprParser termsParser operatorTable


 --   char '\"'
   -- str <- many anySingle
    --char '\"'
    --return ((StringLiteral . show) $ str)

--boolParser = BoolLiteral <$> ((string' "true" >> return True) <|> (string' "false" >> return False) )