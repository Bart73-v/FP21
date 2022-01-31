module AST where

--Daan Eijkman
--Bart Veldman

-- this template uses infix constructors; feel free to use AST2.hs (which uses prefix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+: 
infixl 6 :-: 
infixl 7 :/:
infixl 7 :*:

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
--fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error s)  = Error s
  fmap f (Okay a)   = Okay (f a)

instance Applicative Result where
-- pure :: a -> Result a
  pure x = Okay x
-- (<*>) :: Result (a -> b) -> Result a -> Result b
  (Error s) <*> (Error s')  = Error (s ++ s')
  (Error s) <*> _           = Error s
  (Okay f) <*> x            = fmap f x


eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a 
eval (Lit k) _ = Okay (fromInteger k) 
eval (x :+: y) vars = (+) <$> eval x vars <*> eval y vars
                       
eval (x :-: y) vars = (-) <$> eval x vars <*> eval y vars

eval (x :*: y) vars = (*) <$> eval x vars <*> eval y vars

eval (x :/: y) vars = case (eval x vars, eval y vars) of 
                     (Okay  _, Okay 0)  -> Error ["division by zero"]
                     (Okay x', Okay y') -> Okay (x'/y') 
                     (a,b) -> (/) <$> a <*> b

eval (Var name) vars = if filtered == [] then Error ["Unknown variable: " ++ name] else Okay (snd (filtered !! 0))
  where filtered = filter (\(x,y) -> x==name) vars