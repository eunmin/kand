module Parser (parse) where

import Text.Read
import Type
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


parseTokenList :: TokenList -> Exp

parseTokenList (TokenList (Token "if":pred:cons:alter:[])) =
  If (parseTokenList pred) (parseTokenList cons) (parseTokenList alter)

parseTokenList (TokenList (Token "def":name:body:[])) =
  Def (parseTokenList name) (parseTokenList body)

parseTokenList (TokenList (Token "fn":TokenList args:body:[])) =
  Lambda (map parseTokenList args) (parseTokenList body)
         
parseTokenList (TokenList xs) = Application $ map parseTokenList xs

parseTokenList (Token s) =
  case (readMaybe s :: Maybe Double) of
    Just n -> Nm n
    Nothing -> Sym s


parseRootTokenList :: TokenList -> [Exp]
parseRootTokenList (TokenList tokens) = map parseTokenList tokens

parseRootTokenList EmptyTokenList = []


parse :: String -> [Exp]
parse s = parseRootTokenList $ (fst $ tokenize s "" EmptyTokenList)
