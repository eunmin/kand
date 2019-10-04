data Exp a = Nm a
          | Lst [Exp a]
          | PlusFn
          deriving (Show, Eq)

instance Functor Exp where
  fmap f (Nm a) = Nm (f a)

-- only Nm
instance Applicative Exp where  
    pure = Nm
    (Nm f) <*> nm = fmap f nm
  
eval :: (Num a) => Exp a -> Exp a
eval (Lst (PlusFn:xs)) = foldl (\acc x -> (+) <$> acc <*> (eval x)) (Nm 0) xs

eval x = x

-- (+ 10 (+ 10 10))
main =
  putStrLn $ show (eval $ Lst [PlusFn, (Nm 10), Lst [PlusFn, (Nm 10), (Nm 10)]])
