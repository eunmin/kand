module Core ( plus
            , minus
            , lt
            , le 
            , gt
            , ge
            , eq ) where

import Type

plus :: [Exp] -> Exp
plus ((Nm x):(Nm y):[]) = Nm (x + y)
plus _ = Error "Invalid args"

minus :: [Exp] -> Exp
minus ((Nm x):(Nm y):[]) = Nm (x - y)
minus _ = Error "Invalid args"

lt :: [Exp] -> Exp
lt ((Nm x):(Nm y):[]) = Boolean (x < y)

le :: [Exp] -> Exp
le ((Nm x):(Nm y):[]) = Boolean (x <= y)

gt :: [Exp] -> Exp
gt ((Nm x):(Nm y):[]) = Boolean (x > y)

ge :: [Exp] -> Exp
ge ((Nm x):(Nm y):[]) = Boolean (x >= y)

eq :: [Exp] -> Exp
eq ((Nm x):(Nm y):[]) = Boolean (x == y)

