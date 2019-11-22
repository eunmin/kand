module Lib
    ( someFunc
    , parseExp ) where

import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

wordParser :: Parsec String () String
wordParser = many (noneOf " ")

wordsParser :: Parsec String () [String]
wordsParser = (:) <$> wordParser <*> many (char ' ' *> wordParser)

parseExp = parse wordsParser ""
