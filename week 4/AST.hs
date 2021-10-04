module AST where
import Data.Maybe

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | VarX

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval expr b =
           if parse expr b == parse (Div (Lit 1) (Lit 0)) b
              then Nothing
           else Just (parse expr b)
                where parse (Add e1 e2) b = (parse e1 b) + (parse e2 b)
                      parse (Sub e1 e2) b = (parse e1 b) - (parse e2 b)
		      parse (Mul e1 e2) b = (parse e1 b) * (parse e2 b)
		      parse (Div e1 e2) b = (parse e1 b) / (parse e2 b)
		      parse (Lit i) b     = realToFrac i
		      parse VarX b        = b
