module Main where

import Data.Char

 
-- +, *, 0.., App

data MiniProgram = Lit Integer
  | Plus
  | Mult
  | App2 MiniProgram MiniProgram MiniProgram 
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

parse ('(':cs) = (App2 arg1 arg2 arg3, csfinal)
  where (arg1,cs') = parse cs
        (arg2,cs'') = parse cs'
        (arg3,')':csfinal) = parse cs''
        
parse ('P':cs) = (Plus, cs)
parse ('M':cs) = (Mult, cs)

parse (c:cs) = 
  if (c <= '9')&&(c >= '0') then
    (Lit (toInteger ((ord c) - (ord '0'))), cs)
  else
    error "unknown"

--input = App2 Plus (Lit 5) (Lit 6)
--input2 = App2 Plus input (Lit 9)

eval (Lit n) = Lit n

eval (App2 Plus (Lit n) (Lit m)) = Lit (n+m)
eval (App2 Mult (Lit n) (Lit m)) = Lit (n*m)

eval (App2 mpop mp1 mp2) = eval (App2 mpop (eval mp1) (eval mp2))


x = eval (square (App2 Plus (Lit 1)(Lit 2))) where square x = App2 Mult x x
