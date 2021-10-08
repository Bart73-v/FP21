import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--Daan Eijkman
--Bart Veldman

wordFrequency :: String -> [(String,Int)]
--5.8.1:
--wordFrequency  = map (\x->(head x,length x)) . group . sort . words

--5.8.4: ??? Can't figure it out
--wordFrequency = . M.fromListWith () . map (\x -> (1, x)) . words

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength n = map fst . sortOn snd . filter (\(s,x) -> length s > n) . wordFrequency

wordLengthFrequency :: String -> [(Int, Int)]
wordLengthFrequency = map (\x -> (head x, length x)) . group . sort . map length . words


-- will group at most 2 anagrams (???)
anagrams :: String -> [[String]]
anagrams = groupBy anagram' . map head . group . sort . words

anagram' :: String -> String -> Bool
anagram' xs ys = sort xs == sort ys


{- this 'main' function is only here for the compiled, stand-alone version 
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats 
 -
 - (The -O flag turns on optimizations)
 -}    
main :: IO ()
main = onStdin $ mostFrequentOfLength 5  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))
