module K6
import Data.Nat
import Data.List
import Data.Stream

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

Eq Rat where
    (==) (a :/: b) (c :/: d) = (a * d) == (b * c)

Num Rat where
    (+)(a :/: b)(c :/: d) = norm (a * d + b * c :/: (b * d))
    (*)(a :/: b)(c :/: d) = norm (a * c :/: (b * d))
    fromInteger n = cast n :/: 1

Fractional Rat where
    (/) (a  :/: b)(c :/: d) = norm (a*d :/: b*c)
    recip (a :/: b) = b :/: a

Ord Rat where
    compare (a :/: b) (c :/: d) =
        let esimene = a * d
            teine = b * c
        in
        if esimene < teine then LT else if esimene > teine then GT
        else EQ

infixl 8 -.
interface Monus t where
  (-.) : t -> t -> t

Neg a => Monus a where
  x -. y = x - y

Monus Nat where
  (-.) (S k) 0 = S k
  (-.) (S k) (S l) = (-.) k l
  (-.) _ _ = 0

Monus Rat where
  (-.) (a :/: b) (c :/: d) = norm ((-.) (a * d) (b * c) :/: b * d)

----------------------------------------------------------------

iter : (a -> a) -> a -> Stream a
iter f x = x :: iter f (f x)

while : (a -> Bool) -> Stream a -> List a
while p (x :: xs) = if p x then x :: while p xs else []

inc1 : Rat -> Rat
inc1 (a :/: b) = norm ((a + b) :/: b)

samm : Rat -> Rat -> Rat
samm (a :/: b) (c :/: d) = norm (((-.) (a * d)  (c * b)) :/: (b * d))

lisaSamm : Rat -> Rat -> Rat
lisaSamm (a :/: b) (c :/: d) = norm ((a * d + b * c) :/: (b * d))

Range Rat where
  rangeFrom (a :/: b) = iter inc1 (a :/: b)
  rangeFromTo (a :/: b) (c :/: d) = while (<= (c :/: d)) (rangeFrom (a :/: b))
  rangeFromThen (a :/: b) (c :/: d) = 
      let step = samm (c :/: d) (a :/: b)
      in iter (lisaSamm step) (a :/: b)
  rangeFromThenTo (a :/: b) (c :/: d) (e :/: f) = 
      while (<= (e :/: f)) (rangeFromThen (a :/: b) (c :/: d))

----------------------------------------------------------------

f : Int -> String
f x = show x  -- Simple function to convert Int to String

main : IO ()
main = do
  Prelude.IO.print (take 2 (map f [0..10000]))
