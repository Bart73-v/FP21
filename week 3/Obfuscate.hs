module Obfuscate where

import Data.Char
import Data.List

--Daan Eijkman
--Bart Veldman

cambridge :: String -> String
cambridge xs = unwords (map jumble (words xs))

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

jumble (x:xs)
       | xs == []  = [x]
       | otherwise = [x] ++ (jumble' (tail (reverse xs))) ++ [(head (reverse xs))]

jumble' []           = []
jumble' [x]          = [x]
jumble' (x1:x2:xs)    
       | xs == []  = [x1,x2]
       | otherwise = permutations (x1:x2:xs) !! 3