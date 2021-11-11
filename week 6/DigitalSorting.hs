module DigitalSorting where

--Daan Eijkman
--Bart Veldman

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either
import Deck

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd) . groupBy (\x y -> fst x == fst y) . sortOn fst

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

-- Not sure if this is the efficient way since I don't think this is done in one pass, but in two passes.
instance Rankable Bool where
  rank xs = [[ v | (k,v) <- xs, k == True ], [ v | (k,v) <- xs, k == False]]

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
-- rank :: (Rankable key1, Rankable key2) => [((key1,key2),a)] -> [[a]]
--  rank = concat . map genericRank . genericRank . map assoc             ????

assoc :: ((k1,k2),a) -> (k1,(k2,a))
assoc ((n,m), v) = (n,(m,v))


instance (Rankable key) => Rankable (Maybe key) where
-- rank :: (Rankable key) => [(Maybe key,a)] -> [[a]]
--  rank = 

instance (Rankable key) => Rankable [key] where
--  rank =

-- uncons :: [a] -> Maybe (a, [a])

--rankWithKey :: (Rankable key) => [(key,a)] -> [[(key,a)]]
--rankWithKey = groupBy (\x y -> fst x == fst y) . sortOn fst


