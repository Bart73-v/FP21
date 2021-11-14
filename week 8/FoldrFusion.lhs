> module FoldrFusion where
--Daan Eijkman
--Bart Veldman
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that foldr g e (\x xs -> f x : xs) = (g . f) x ((foldr g e) y) holds.

{-
f (g x y) = h x (f y)
foldr g e (\x xs -> f x : xs) = (g . f) x ((foldr g e) y)

foldr g e (\x y->f x : y) = g . f x (foldr g e y)

f = foldr g e 
g = \x xs -> f x : xs
e = []
h = g . f
-}

Which is the case since:

  foldr g e (\x y->f x : y)
  -------------------------
= 
  ????
= 
  -------------------------
= g . f x (foldr g e y)

--------------------------------------
To prove:  map (f . g) = map f . map g

-- foldr-map fusion law: foldr g e . map f = foldr (g . f) e
-- (\x xs-> f x : xs) . g = (\x xs-> f (g x) : xs)
-- map f = foldr (\x xs->f x : xs) []

    map (f . g)
    -----------------------------------   map as foldr
  = foldr (\x xs-> (f . g) x : xs) []
    -----------------------------------

    ???

    -----------------------------------
  = foldr ((\x xs-> f x : xs) . g) []
    -----------------------------------   foldr-map fusion
  = foldr (\x xs-> f x : xs) [] . map g
    -----------------------------------   map as foldr
  = map f . map g
----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

