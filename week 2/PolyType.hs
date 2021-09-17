module PolyType where

f8 x y  = if x <= y then x else y

f9 x y  = not x || y

f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 x y = get 0
  where get n = if n == 0 then x else y

-- 2.4.1
-- f8 and f11 will accept String arguments

-- 2.4.2
-- f8 = ad-hoc polymorphism
-- f9 = not polymorphic, type :: Bool -> Bool -> Bool
-- f10 = ad-hoc polymorphism
-- f11 = parametric polymorhpic
