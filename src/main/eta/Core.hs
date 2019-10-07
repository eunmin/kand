module Core (
  plus,
  minus
  ) where

import Type

plus :: [Exp] -> Exp
plus ((Nm x):(Nm y):[]) = Nm (x + y)
plus _ = Error "Invalid args"

minus :: [Exp] -> Exp
minus ((Nm x):(Nm y):[]) = Nm (x - y)
minus _ = Error "Invalid args"

