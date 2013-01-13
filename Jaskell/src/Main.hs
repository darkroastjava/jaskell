module Main where

import Data.Char
import Debug.Trace
 
-- +, *, 0.., App

data MiniWert = Num Integer
  | Plus
  | Mult
  | Func Char Char [(Char, MiniWert)] [Char] String
  deriving Show

--"2"
--"P"
--"(PMM)"
--"(234)"
--"(P(P13)(M12))"
--"((P13)(M12)4)"
--"=v5(Pvv)"
--"=dFpq{(Mp2)}(d30)" ~> Num 6
--"=dFpq{(Mp=tFxy{2}(t00))}(d30)" ~> Num 6

call Plus (Num m) (Num n) = Num (m+n)
call Mult (Num m) (Num n) = Num (m*n)
call f x y = 
  call' f
  where
    call' (Func p q env names prog) = res 
      where  
        (res, "") = 
          traceStack ("parseval with: "++(show x)) (parseval prog ((p,x):(q,y):(map (\name -> (name,f)) names) ++ env))

-- assoc :: (Eq a) => a -> [(a, t)] -> t

assoc key [] = error "Key not found in context"

assoc key ((var,val):ctx) = 
  if (key == var) then val else assoc key ctx

parsecurly ('}':cs) = ("", cs)

parsecurly (' ':cs) = parsecurly cs

parsecurly ('{':cs) = ('{':subprog ++ '}':progrest, csfinal)
  where (subprog, cs') = parsecurly cs
        (progrest, csfinal) = parsecurly cs'

parsecurly (c:cs) = (c:prog, csfinal)
  where (prog, csfinal) = parsecurly cs

parseval :: [Char] -> [(Char, MiniWert)] -> (MiniWert, [Char])

parseval ('F':p:q:'{':cs) ctx = (Func p q ctx [] prog, csfinal)
  where (prog,csfinal) = parsecurly cs

parseval ('(':cs) ctx = (call arg1 arg2 arg3, csfinal)
  where (arg1,cs') = parseval cs ctx
        (arg2,cs'') = parseval cs' ctx
        (arg3,')':csfinal) = parseval cs'' ctx

parseval ('=':var:cs) ctx = parseval cs' ((var,valfinal):ctx)
  where (val,cs') = parseval cs ctx
        valfinal = amend val
          where
            amend (Func p q env names prog) = (Func p q env (var:names) prog)
            amend val = val

parseval ('P':cs) ctx = (Plus, cs)
parseval ('M':cs) ctx = (Mult, cs)

parseval (' ':cs) ctx = parseval cs ctx
parseval ('\n':cs) ctx = parseval cs ctx

parseval (c:cs) ctx = 
  if (c <= '9')&&(c >= '0') then
    (Num (toInteger ((ord c) - (ord '0'))), cs)
  else
    (assoc c ctx,cs)

main = print (parseval "(=g=fFx_{ (g (Px1) _) }fg 1 _)" [])