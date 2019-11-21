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

tokenize ('(':xs) buffer result =
  let (ast, rest) = tokenize xs "" EmptyTokenList
  in tokenize rest "" (result <> TokenList [ast])

tokenize (')':xs) buffer result =
  ((result <> stringToken buffer), xs)

tokenize (x:xs) buffer result =
  if isSpace x
  then tokenize xs "" (result <> stringToken buffer)
  else tokenize xs (buffer ++ [x]) result

tokenize "" buffer result = ((result <> stringToken buffer), "")
