module Data.Hutton (Expr(..), eval) where
  
import Prelude

data Expr = Lit Int | Add Expr Expr

eval :: Expr -> Int
eval (Lit i)     = i
eval (Add x1 x2) = eval x1 + eval x2