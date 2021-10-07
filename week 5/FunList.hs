module FunList where
--Daan Eijkman
--Bart Veldman

compose :: [a -> a] -> (a -> a)
compose []      = id
compose (f:fs)  = f . compose fs

compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1
-- foo computes the factorial.
-- the function creates a list of functions. Each function is a multiplication with x, where x is [1..n]. 
-- these functions are chained via composition and applied to 1,    e.g. 1 * 2 * 3 * ... * n

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' st ba [] = ba
foldr' st ba xs = compose (map st xs) ba
