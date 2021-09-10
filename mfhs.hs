--double x = x + x
--quadruple x = double (double x)
--fac n = product [1 .. n]
--facOdd n = product [1,3 .. n]

pyramid :: String -> String
pyramid str = pyram 0 str where 
    pyram :: Int -> String -> String
    pyram n str 
      | n < (length str + 1) `div` 2 = spaces ++ substr ++ newline ++ pyram (n+1) str
      | otherwise                    = "" 
      where 
          spaces = replicate n ' '
          substr = reverse (drop n (reverse (drop n str)))
          newline = "\n"


printPyramid = putStr (pyramid "Functional programming is fun")

--double x = incr (incr 0)
--  where incr y = x + y

