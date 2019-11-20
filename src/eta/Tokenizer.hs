module Tokenizer ( TokenList(..)
                 , tokenize ) where

import Data.Char

data TokenList = Token String
               | TokenList [TokenList]
               | EmptyTokenList deriving Show


instance Semigroup TokenList where
  TokenList x <> TokenList y = TokenList (x ++ y)
  TokenList x <> EmptyTokenList = TokenList x
  EmptyTokenList <> TokenList x = TokenList x
  EmptyTokenList <> EmptyTokenList  = EmptyTokenList


stringToken :: String -> TokenList

stringToken "" = EmptyTokenList

stringToken s = TokenList [Token s]

tokenize :: String -> String -> TokenList -> (TokenList, String)

tokenize (x:xs) buffer result
  | x == '(' = let (ast, rest) = tokenize xs "" EmptyTokenList
               in tokenize rest "" (result <> TokenList [ast])
  | x == ')' = ((result <> stringToken buffer), xs)                
  | isSpace x = tokenize xs "" (result <> stringToken buffer)
  | otherwise = tokenize xs (buffer ++ [x]) result
  
tokenize "" buffer result = ((result <> stringToken buffer), "")

