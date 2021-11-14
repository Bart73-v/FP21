module Expression where
--Daan Eijkman
--Bart Veldman

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs

  Case t = Leaf:

    inorder Leaf ++ xs
    ---------------- definition of inorder
  = [] ++ xs
    ---------------- definition of ++
  = xs
    ---------------- definition of inorderCat
  = inorderCat Leaf xs

  ---> inorderCat Leaf xs = xs

  Case t = Node x lt rt
  IH1 = inorderCat lt xs = inorder lt ++ xs
  IH2 = inorderCat rt xs = inorder rt ++ xs

    inorder (Node x lt rt) ++ xs
    ---------------------------- definition of inorder
  = inorder lt ++ [x] ++ inorder rt ++ xs
    ---------------------------- IH2
  = inorder lt ++ [x] ++ inorderCat rt xs
    ---------------------------- IH1
  = inorderCat lt [x] ++ inorderCat rt xs
    ---------------------------- definition of inorderCat
  = inorderCat (Node x ls rs) xs

  ---> inorderCat (Node x lt rt) xs = inorderCat lt [x] ++ inorderCat rt xs
-}

inorderCat :: Tree a -> [a] -> [a]
--inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient
inorderCat Leaf xs            = xs
inorderCat (Node x lt rt) xs  = inorderCat lt [x] ++ inorderCat rt xs

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- is it more efficient?
-- I have reduced the nr. of ++ uses by 1 so I would expect inorder' to be more efficient


elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x lt rt) = x : elemsCat lt (elemsCat rt xs)

elems' :: Tree a -> [a]
elems' t = elemsCat t []

{-
  elemsCat t xs = elems t ++ xs

  Case t = Leaf:

    elems Leaf ++ xs
    ---------------- definition of elems
  = [] ++ xs
    ---------------- definition of ++
    xs
    ---------------- definition of elemsCat
  = elemsCat Leaf xs

  Case t = Node x lt rt
  IH1 = elemsCat lt xs = elems lt ++ xs
  IH2 = elemsCat rt xs = elems rt ++ xs

    elems (Node x lt rt) ++ xs
    -------------------------- definition of elems
  = x : elems lt ++ elems rt ++ xs
    -------------------------- IH2
  = x : elems lt ++ elemsCat rt xs
    -------------------------- IH1
  = x : elemsCat lt (elemsCat rt xs)
    -------------------------- definition of elemsCat
  = elemsCat (Node x lt rt) xs

-}
