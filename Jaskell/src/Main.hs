module Main where

import Data.Char

 
-- +, *, 0.., App

data MiniWert = Num Integer
  | Plus
  | Mult
  deriving Show

--"2"
--"P"
--"(PMM)"
--"(234)"
--"(P(P13)(M12))"
--"((P13)(M12)4)"
--"=v5(Pvv)"

input = "P56"


--parse ('P':c1:c2:[]) = App2 Plus (parse [c1]) (parse [c2])

--parse ('(':cs) = (App2 arg1 arg2 arg3, csfinal)
--  where (arg1,cs') = parse cs
--        (arg2,cs'') = parse cs'
--        (arg3,')':csfinal) = parse cs''

call Plus (Num m) (Num n) = Num (m+n)
call Mult (Num m) (Num n) = Num (m*n)

assoc key [] = error "Key not found in context"

assoc key ((var,val):ctx) = 
  if (key == var) then val else assoc key ctx


parseval ('(':cs) ctx = (call arg1 arg2 arg3, csfinal)
  where (arg1,cs') = parseval cs ctx
        (arg2,cs'') = parseval cs' ctx
        (arg3,')':csfinal) = parseval cs'' ctx

parseval ('=':var:cs) ctx = parseval cs' ((var,val):ctx)
  where (val,cs') = parseval cs ctx

parseval ('P':cs) ctx = (Plus, cs)
parseval ('M':cs) ctx = (Mult, cs)

parseval (c:cs) ctx = 
  if (c <= '9')&&(c >= '0') then
    (Num (toInteger ((ord c) - (ord '0'))), cs)
  else
    (assoc c ctx,cs)

