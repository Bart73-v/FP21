> module TreeInduction where
--Daan Eijkman
--Bart Veldman
> 
> data Tree a = Leaf | Node a (Tree a) (Tree a)
>   deriving (Show)

1: What is the induction scheme for trees?
Base case: show property holds for Leaf
    P(LeaF)

Inductive step: assuming property holds for some Tree, show it holds for the Node of two trees:
    P(Tree) -> for all x, P(Node x (Tree) (Tree))

Local definitions:

> leaves :: Tree a -> Int
> leaves Leaf = 1
> leaves (Node _ l r) = leaves l + leaves r
>
> nodes :: Tree a -> Int
> nodes Leaf = 0
> nodes (Node _ l r) = 1 + nodes l + nodes r

2: To prove: leaves t = nodes t + 1
By induction on t.

Case 1: t = Leaf

    leaves Leaf
    -----------  definition of leaves
  = 1
    -----------  ??? def of 1?
  = 0 + 1
    -----------  definition of nodes
  = nodes Leaf + 1

Case 2: t = Node x (Tree a) (Tree b)
IH: leaves Tree = nodes Tree + 1

    leaves Node x (Tree a) (Tree b)
    -------------------------------
  =  
