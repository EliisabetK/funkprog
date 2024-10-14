module K6
import Data.Nat

data Tree a = Leaf | Branch (Tree a) a (Tree a)
 
Eq a => Eq (Tree a) where
  Leaf == Leaf = True
  (Branch x y z) == (Branch w v s) =  x==w && y==v && z==s
  _ == _ = False
 
Functor Tree where
  map _ Leaf = Leaf
  map f (Branch v x p) = Branch (map f v) (f x) (map f p)

----------------------------------------------------------------

Foldable Tree where
  foldr f b Leaf = b
  foldr f b (Branch v x p) = foldr f (f x (foldr f b p)) v

----------------------------------------------------------------

len : Foldable t => t a -> Int
len = foldr (\x, pikkus => 1 + pikkus) 0

----------------------------------------------------------------

infix 7 :/:
data Rat = (:/:) Nat Nat
 
-- normaliseerimine
norm : Rat -> Rat
norm (_   :/:   0) = 0 :/: 0
norm (0   :/:   _) = 0 :/: 1
norm (S a :/: S b) =
    let n = gcd (S a) (S b) in
    (S a) `div` n :/: (S b) `div` n
 
-- muud operatsioonid:
-- (a :/: b) == (c :/: d) = a*d == b*c
-- (a :/: b) +  (c :/: d) = a*d + b*c :/: b*d
-- (a :/: b) *  (c :/: d) = a*c :/: b*d
-- (a :/: b) /  (c :/: d) = a*d :/: b*c
-- pöörd (a :/: b) = b :/: a
 
neljandik : Rat
neljandik = 1 :/: 4
 
pool : Rat
pool = 1 :/: 2

----------------------------------------------------------------



----------------------------------------------------------------



----------------------------------------------------------------



----------------------------------------------------------------
