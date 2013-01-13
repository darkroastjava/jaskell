module Main where

import Data.Char

 
-- +, *, 0.., App

data MiniWert = Num Integer
  | Plus
  | Mult
  | Func Char Char String
  deriving Show

--"2"
--"P"
--"(PMM)"
--"(234)"
--"(P(P13)(M12))"
--"((P13)(M12)4)"
--"=v5(Pvv)"
--"=dFpq{(Mp2)}(d30)" ~> Num 6

call Plus (Num m) (Num n) = Num (m+n)
call Mult (Num m) (Num n) = Num (m*n)
call (Func p q prog) x y = res 
  where (res, "") = parseval prog ((p,x):(q,y):[])

-- assoc :: (Eq a) => a -> [(a, t)] -> t

assoc key [] = error "Key not found in context"

assoc key ((var,val):ctx) = 
  if (key == var) then val else assoc key ctx

parsecurly ('}':cs) = ("", cs)

parsecurly (c:cs) = (c:prog, csfinal)
  where (prog, csfinal) = parsecurly cs

parseval :: [Char] -> [(Char, MiniWert)] -> (MiniWert, [Char])

parseval ('F':p:q:'{':cs) ctx = (Func p q prog, csfinal)
  where (prog,csfinal) = parsecurly cs

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

