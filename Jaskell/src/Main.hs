module Main where

-- +, *, 0.., App

data MiniProgram = Lit Integer
  | Plus
  | Mult
  | App2 MiniProgram MiniProgram MiniProgram 
  deriving Show


input = App2 Plus (Lit 5) (Lit 6)
input2 = App2 Plus input (Lit 9)

eval (Lit n) = Lit n
eval (App2 Plus (Lit n) (Lit m)) = Lit (n+m)
eval (App2 Plus mp1 mp2) = eval (App2 Plus (eval mp1) (eval mp2))
