module Parser (parse) where

import Text.Read
import Type
import Data.Char

data TokenBuffer = TokenBuffer String | TokenNotReady | EmptyToken

data AST = Token String | TokenList [AST] | EmptyAST deriving Show

instance Semigroup AST where
  TokenList x <> TokenList y = TokenList (x ++ y)
  TokenList x <> EmptyAST = TokenList x
  EmptyAST <> TokenList x = TokenList x

parseList :: String -> TokenBuffer -> AST -> (AST, String)
 
parseList (x:xs) TokenNotReady result
  | x == '(' = parseList xs EmptyToken result
  | otherwise = parseList xs TokenNotReady result

parseList (x:xs) (TokenBuffer s) result
  | x == '(' = let (ast, rest) = parseList (x:xs) TokenNotReady EmptyAST in
      parseList rest EmptyToken (result <> TokenList [Token s, ast])
  | x == ')' = (result <> TokenList [Token s], xs)
  | isSpace x = parseList xs EmptyToken (result <> TokenList [Token s])
  | otherwise = parseList xs (TokenBuffer (s ++ [x])) result
  
parseList (x:xs) EmptyToken result
  | x == '(' = let (ast, rest) = parseList (x:xs) TokenNotReady EmptyAST in
      parseList rest EmptyToken (result <> TokenList [ast])
  | x == ')' = (result, xs)
  | isSpace x = parseList xs EmptyToken result
  | otherwise = parseList xs (TokenBuffer [x]) result


parseAST :: AST -> Exp

parseAST (TokenList (Token "if":pred:cons:alter:[])) =
  If (parseAST pred) (parseAST cons) (parseAST alter)

parseAST (TokenList (Token "def":name:body:[])) =
  Def (parseAST name) (parseAST body)

parseAST (TokenList (Token "fn":TokenList args:body:[])) =
  Lambda (map parseAST args) (parseAST body)
         
parseAST (TokenList xs) = Application $ map parseAST xs

parseAST (Token s) =
  case (readMaybe s :: Maybe Double) of
    Just n -> Nm n
    Nothing -> Sym s

parse :: String -> Exp
parse s = parseAST $ (fst $ parseList s TokenNotReady EmptyAST)

  

