--Daan Eijkman
--Bart Veldman
-----------------------------------------------------
To prove: map (f . g) xs = map f (map g xs)
By induction on xs.

Case 1: xs = []

    map (f . g) []
    --------------  defintion of map
  = []
    --------------  definition of map
  = map f []
    --------------  definition of map
  = map f (map g [])

Case 2: xs = (a:as)
IH: map (f . g) as = map f (map g as), for all f and g

    map (f . g) (a:as)
    ------------------  definition of map
  = (f . g) a : map (f . g) as
    ------------------  IH
  = (f . g) a : map f (map g as)
    ------------------  definition of (.)
  = f (g a) : map f (map g as)
    ------------------  definition of map
  = map f (g a : map g as)
    ------------------ definition of map
  = map f (map g (a:as))


-----------------------------------------------------
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
By induction on ...

Case 1: ...

Case 2: ...
IH: ...

-----------------------------------------------------
To prove: concat (map (map f) xs) = map f (concat xs)

...

