triangle :: Int -> String
triangle n = tri (n-1) 1 ++ replicate (n*2-1) '*' ++ "\n"

tri :: Int -> Int -> String
tri x y 
    | x > 0     = tri (x-1) (y+1) ++ replicate y ' ' ++ replicate (x*2-1) '*' ++ "\n"
    | otherwise = ""

triangle' :: Int -> Int -> String
triangle' n m = tri' (n-1) 1 m ++ replicate m ' ' ++ replicate (n*2-1) '*' ++ "\n"

tri' :: Int -> Int -> Int -> String
tri' x y z
    | x > 0     = tri' (x-1) (y+1) z ++ replicate z ' ' ++ replicate y ' ' ++ replicate (x*2-1) '*' ++ "\n"
    | otherwise = ""

christmasTree :: Int -> String 
christmasTree n  = ctree (n-1) 1 ++ triangle' n 0

ctree :: Int -> Int -> String
ctree n m 
  | n > 0     = ctree (n-1) (m+1) ++ triangle' n m
  | otherwise = ""