module Parser (parse) where

import Text.Read
import Type
import Data.Char

data TokenBuffer = TokenBuffer String | TokenNotReady | EmptyToken

parseList :: String -> TokenBuffer -> [Exp] -> (Exp, String)
 
parseList (x:xs) TokenNotReady result
  | x == '(' = parseList xs EmptyToken result
  | otherwise = parseList xs TokenNotReady result

parseList (x:xs) (TokenBuffer s) result
  | x == '(' = let (exp, rest) = parseList (x:xs) TokenNotReady [] in
      parseList rest EmptyToken (result ++ [parseToken s, exp])
  | x == ')' = (Application (result ++ [parseToken s]), xs)
  | isSpace x = parseList xs EmptyToken (result ++ [parseToken s])
  | otherwise = parseList xs (TokenBuffer (s ++ [x])) result
  
parseList (x:xs) EmptyToken result
  | x == '(' = let (exp, rest) = parseList (x:xs) TokenNotReady [] in
      parseList rest EmptyToken (result ++ [exp])
  | x == ')' = (Application result, xs)
  | isSpace x = parseList xs EmptyToken result
  | otherwise = parseList xs (TokenBuffer [x]) result

parseToken :: String -> Exp
parseToken s =
  case (readMaybe s :: Maybe Double) of
    Just n -> Nm n
    Nothing -> Sym s

parse :: String -> Exp
parse s = fst $ parseList s TokenNotReady []

