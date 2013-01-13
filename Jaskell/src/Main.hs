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

parseval ('(':cs) = (call arg1 arg2 arg3, csfinal)
  where (arg1,cs') = parseval cs
        (arg2,cs'') = parseval cs'
        (arg3,')':csfinal) = parseval cs''
        
parseval ('P':cs) = (Plus, cs)
parseval ('M':cs) = (Mult, cs)

parseval (c:cs) = 
  if (c <= '9')&&(c >= '0') then
    (Num (toInteger ((ord c) - (ord '0'))), cs)
  else
    error "unknown"
