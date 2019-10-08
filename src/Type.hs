module Type
  (Exp(..)
  ) where

data Exp = Nm Double
          | Fn ([Exp] -> Exp)
          | Sym String
          | Lst [Exp]
          | Error String 

instance Semigroup Exp where
  Error x <> Error y = Error (x ++ " " ++ y)

instance Monoid Exp where
  mempty = Error ""

instance Show Exp where
  show = showExp

showExp :: Exp -> String
showExp (Nm n) = show n
showExp (Fn f) = "function"
showExp (Lst _) = "list"
showExp (Sym name) = "symbol " ++ name
showExp (Error message) = "Error: " ++ message
