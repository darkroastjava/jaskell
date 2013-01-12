--module Main where



--main::IO()
--main = undefined


rep = \ x -> x:rep x

nimm1 = \ (x:xs) -> x

--nimm = \ n (x:xs) -> if n==0 then [] else x:(nimm (n-1) xs)
nimm 0 xs = []
nimm n (x:xs) = x:(nimm (n-1) xs)
--nimm n [] = error "leere liste"
nimm n [] = []

