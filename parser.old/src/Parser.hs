module Parser ( parse ) where

import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

data Expr = ExprList [Expr]
          | ExprSymbol String
          | ExprNum Integer
          | ExprString String
          | ExprQuoted
          | ExprIf
          | ExprDef
          | ExprTrue
          | ExprFalse
          | ExprFn
          deriving (Show)

whitespaces :: Parsec.Parser String
whitespaces = Parsec.many Parsec.space

number :: Parsec.Parser Expr
number = ExprNum . read <$> Parsec.many1 Parsec.digit

string :: Parsec.Parser Expr
string = do
  Parsec.char '"'
  x <- Parsec.many1 (Parsec.noneOf "\"")
  Parsec.char '"'
  return (ExprString x)

symbol :: Parsec.Parser Expr
symbol = do
  x <- Parsec.many1 (Parsec.noneOf "() ")
  return $ case x of
             "true" -> ExprTrue
             "false" -> ExprFalse
             "fn" -> ExprFn
             "if" -> ExprIf
             "def" -> ExprDef
             "quote" -> ExprQuoted
             _ -> ExprSymbol x

quoted :: Parsec.Parser Expr
quoted = do
  Parsec.char '\''
  x <- expr
  return $ ExprList [ExprQuoted, x]

list :: Parsec.Parser Expr
list = do
  Parsec.char '('
  whitespaces
  x <- Parsec.sepEndBy expr whitespaces
  whitespaces
  Parsec.char ')'
  return (ExprList x)

expr :: Parsec.Parser Expr
expr = Parsec.try number <|>
       Parsec.try string <|>
       Parsec.try quoted <|>
       Parsec.try symbol <|>
       list

exprs :: Parsec.Parser [Expr]
exprs = Parsec.sepEndBy expr whitespaces

parse :: String -> Either Parsec.ParseError [Expr]
parse = Parsec.parse exprs ""
