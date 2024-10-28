yl4 : List (Int, Bool) -> Maybe Nat
yl4 xs = 
  let sum = foldr f 0 xs
  in if sum >= 0 && sum < length xs then Just sum else Nothing
  where
    f : (Int, Bool) -> Nat -> Nat
    f (a, b) acc = if a == 1 && not b then acc else acc + 1


interface F a where
  f : a -> a -> a

F Bool where
  f x y = not(x&&y)

F (List a) where
  f xs ys = xs ++ ys


data Tree a = LeafJust a | LeafNothing | Branch (Tree a) (Tree a)

test_tree : Tree (Char, Int)
test_tree =
  Branch (LeafJust ('x', 1))
    (Branch (Branch LeafNothing (LeafJust ('y', 2))) (LeafJust ('x', 10)))

find_all : Eq a => a -> Tree (a, b) -> List (b)
find_all x LeafNothing = []
find_all x (LeafJust a) = if fst a == x then [snd(a)] else []
find_all x (Branch left right) = find_all x left ++ find_all x right
