module Parser (parse) where

import Text.Read
import Type
import Tokenizer

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
