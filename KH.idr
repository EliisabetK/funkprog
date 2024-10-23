yl4 : List (Int, Bool) -> Maybe Nat
yl4 xs = let sum = foldr f 0 xs in if not(sum == length xs) then Just sum else Nothing
  where
    f : (Int, Bool) -> Nat -> Nat
    f (x, b) acc = if x == 1 && not b then acc else acc + 1




data Tree a = LeafJust a | LeafNothing | Branch (Tree a) (Tree a)

test_tree : Tree (Char, Int)
test_tree =
    Branch (LeafJust ('x', 1))
        (Branch (Branch LeafNothing (LeafJust ('y', 2))) (LeafJust ('x',10)))


find_all : Eq a => a -> Tree (a, b) -> List b
find_all x (LeafJust (a, b)) = if x == a then [b] else []
find_all _ LeafNothing = []
find_all x (Branch v p) = find_all x v ++ find_all x p
