Local definitions:
--Daan Eijkman
--Bart Veldman

> import Prelude hiding (foldr)

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)
>
> compose :: [a -> a] -> a -> a
> compose [] = id
> compose (f:fs) = f . compose fs

> map :: (a -> b) -> [a] -> [b]
> map f []      = []
> map f (x:xs)  = f x : map f xs

-----------------------------------------------------
To prove: foldr f b xs = compose (map f xs) b
By induction on xs.

Case 1: xs = []

    foldr f b []
    ------------  definition of foldr
  = b
    ------------  definition of id
  = id b
    ------------  definition of compose
  = compose [] b
    ------------  definition of map
  = compose (map f []) b

Case 2: xs = (x:xs)
IH: foldr f b xs = compose (map f xs) b

    foldr f b (x:xs)
    ----------------    definition of foldr
  = f x (foldr f b xs)
    ----------------    definition of (.)
  = f x . foldr f b xs
    ----------------    IH
  = f x . compose (map f xs) b
    ----------------    definition of compose
  = compose (f x : map f xs) b
    ----------------    defnition of map
  = compose (map f (x:xs)) b
