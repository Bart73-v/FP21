module Tree where

  --Daan Eijkman
--Bart Veldman

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

{----------- exercise 4.3 -------------}

-- 4.3.1
firstTree :: Tree Char
firstTree = Node 'c' (Node 'a' Leaf (Node 'b' Leaf Leaf)) (Node 'f' (Node 'd' Leaf Leaf) (Node 'g' Leaf Leaf))

-- 4.3.2

-- 4.3.3
leaves :: Tree a -> Int
leaves Leaf         = 1
leaves (Node _ l r) = leaves l + leaves r

nodes  :: Tree a -> Int
nodes Leaf          = 0
nodes (Node _ l r)  = 1 + nodes l + nodes r

height :: Tree a -> Int
height Leaf         = 0
height (Node _ l r) = 1 + max (height l) (height r)

elems  :: Tree a -> [a]
elems Leaf          = []
elems (Node x l r)  = elems l ++ [x] ++ elems r

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf         = True
isSearchTree (Node x l r) = maximum (elems l ++ [x]) == x && minimum (elems r ++ [x]) == x && isSearchTree l && isSearchTree r 

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member x Leaf             = False
member x (Node y l r)
    | x < y               = member x l
    | x > y               = member x r
    | x == y              = True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf             = Node x Leaf Leaf
insert x (Node y l r)
  | x == y                = Node y l r
  | x < y                 = Node y (insert x l) r
  | x > y                 = Node y l (insert x r)

fromList :: (Ord a) => [a] -> Tree a
fromList []       = Leaf
fromList (x:xs)   = insert x (fromList xs)

delete :: (Ord a) => a -> Tree a -> Tree a
delete x Leaf         = Leaf
delete x (Node y l r)
  | x < y             = Node y (delete x l) r 
  | x > y             = Node y l (delete x r)
  | x == y            = del' (Node y l r)

del' :: (Ord a) => Tree a -> Tree a
del' Leaf               = Leaf
del' (Node y Leaf Leaf) = Leaf
del' (Node y Leaf r)    = r
del' (Node y l Leaf)    = l
del' (Node y l r)       = Node ioSuc l (delete ioSuc r) -- replace y with its inorder-successor. Delete the inorder-successor from right tree
  where ioSuc = minimum (elems r)


{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a]
inOrder Leaf          = []
inOrder (Node y l r)  = inOrder l ++ [y] ++ inOrder r

fromAscList :: [a] -> Tree a
fromAscList []        = Leaf
fromAscList xs        = Node r (fromAscList l) (fromAscList rs)
  where (l, (r:rs)) = splitAt ((length xs) `div` 2) xs


breadthFirst :: Tree a -> [a]
breadthFirst Leaf         = []
breadthFirst (Node x l r) = bfs' (Node x l r) [] []

-- Current node -> Queue -> Results
bfs' :: Tree a -> [Tree a] -> [a] -> [a]
bfs' (Node y l r) [] ys         = ys
bfs' (Node y l r) xs ys     = bfs' (head queue) queue (ys ++ [y])
  where queue = xs ++ (children l r)

-- get all non-leaf children
children :: Tree a -> Tree a -> [Tree a]
children Leaf Leaf                        = []
children Leaf (Node y l r)                = [(Node y l r)]
children (Node x l r) Leaf                = [(Node x l r)]
children (Node x l r) (Node y l' r')      = [(Node x l r), (Node y l' r')]






{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}


layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  lbend = fill ++ "╰─"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)

