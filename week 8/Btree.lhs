> module Btree where
--Daan Eijkman
--Bart Veldman
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

> map :: (a -> b) -> [a] -> [b]
> map f []      = []
> map f (x:xs)  = f x : map f xs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t


Case: t = Tip a 

    map f (tips (Tip a))
    --------------------    definition of tips
  = map f [a]
    --------------------    definition of ++
  = map f (a:[])
    --------------------    definition of map
  = f a : map f []
    --------------------    definition of map
  = f a : []
    --------------------    definition of ++
  = [f a]
    --------------------    definition of tips
  = tips (Tip (f a))
    --------------------    definition of mapBtree
  = tips (mapBtree f (Tip a))

Case: t = Bin (xs) (ys)
IH1: map f (tips xs) = tips (mapBtree f xs)
IH2: map f (tips ys) = tips (mapBtree f ys)

    map f (tips (Bin (xs) (ys))) 
    ---------------------------------  definition of tips
  = map f (tips xs ++ tips ys)
    ---------------------------------  definition of map (??? Not 100% sure)
  = map f (tips xs) ++ map f (tips ys)
    ---------------------------------  IH1 & IH2
  = tips (mapBtree f xs) ++ tips (mapBtree f ys)
    ---------------------------------  definition of tips
  = tips (Bin (mapBtree f xs) (mapBtree f ys))
    ---------------------------------  definition of mapBtree
  = tips (mapBtree f (Bin (xs) (ys)))


