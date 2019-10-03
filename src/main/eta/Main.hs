data Exp = Num Double
          | Lst [Exp]
          | PlusFn
          deriving (Show, Eq)

value :: Exp -> Double
value (Num n) = n

eval :: Exp -> Exp
eval (Lst (PlusFn:xs)) = Num $ sum (map
                                    (\x -> case x of
                                        (Num n) -> value x
                                        ns -> value $ eval ns)
                                    xs)

eval x = x

-- (+ 10 (+ 10 10))
main =
  putStrLn $ show (eval $ Lst [PlusFn, (Num 10), Lst [PlusFn, (Num 10), (Num 10)]])
