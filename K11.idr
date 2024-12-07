module K11

punkt2d : Type
punkt2d = (Double, Double)

punkt : Nat -> Type
punkt Z     = Unit
punkt (S Z) = Double
punkt (S n) = (Double, punkt n)

xy : punkt 2
xy = (2.0,4.0)
 
xyz : punkt 3
xyz = (3.0,6.0,3.0)

nullpunkt : (d : Nat) -> punkt d
nullpunkt Z = ()
nullpunkt (S Z) = 0.0
nullpunkt (S (S n)) = (0.0, nullpunkt (S n))

add : (d : Nat) -> punkt d -> punkt d -> punkt d
add Z () () = ()
add (S Z) x y = x + y
add (S (S n)) (x1 , xs1) (x2 , xs2) = (x1 + x2 , add (S n) xs1 xs2)

sum : (d : Nat) -> List (punkt d) -> punkt d
sum d xs = foldr (add d) (nullpunkt d) xs

---------------------------------------------------------------------------------------

data Vect : (k : Nat) -> (a : Type) -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a
 
Eq a => Eq (Vect k a) where
    [] == []             = True
    (x :: y) == (z :: w) = x==z && y==w

append : Vect n a -> Vect m a -> Vect (n + m) a
append []        ys = ys
append (x :: xs) ys = x :: append xs ys

zipVect : Vect n a -> Vect n b -> Vect n (a, b)
zipVect [] [] = []
zipVect (x :: xs) (y :: ys) = (x, y) :: zipVect xs ys

replaceVect : a -> (n : Nat) -> Vect (1 + n + k) a -> Vect (1 + n + k) a
replaceVect c Z (x :: xs) = c :: xs
replaceVect c (S n) (x :: xs) = x :: replaceVect c n xs

min : Nat -> Nat -> Nat
min 0 y = 0
min (S k) 0 = 0
min (S k) (S j) = S (min k j)
 
takeVec : (n : Nat) -> {m : Nat} -> Vect m a -> Vect (min n m) a
takeVec Z {m} xs = []
takeVec (S n) {m = Z} [] = []
takeVec (S n) {m = S m} (x :: xs) = x :: takeVec n {m} xs


---------------------------------------------------------------------------------------

data TreeShape : Type where
  LeafShape : TreeShape
  NodeShape : (l : TreeShape) -> (r : TreeShape) -> TreeShape
 
data Tree : TreeShape -> Type -> Type where
  Leaf : Tree LeafShape a
  Node : (left : Tree l a) -> (this : a) -> (right : Tree r a) ->
       Tree (NodeShape l r) a
 
Eq TreeShape where
    LeafShape == LeafShape  = True
    LeafShape == (NodeShape l r)  = False
    (NodeShape l r) == LeafShape  = False
    (NodeShape l r) == (NodeShape x y)  =
        l==x && r==y
 
Eq a => Eq (Tree s a) where
    Leaf == Leaf = True
    (Node left this right) == (Node x y z) =
        left == x && this == y && right == z
 
tr1 : Tree (NodeShape LeafShape (NodeShape LeafShape LeafShape)) Int
tr1 = Node Leaf 1 (Node Leaf 2 Leaf)
 
tr2 : Tree (NodeShape LeafShape (NodeShape LeafShape LeafShape)) Int
tr2 = Node Leaf 3 (Node Leaf 1 Leaf)

-- data Tree : Type -> Type where
--   Leaf : Tree a
--   Node : Tree a -> a -> Tree a -> Tree a
--
-- zip_tree : Tree a -> Tree b -> Tree (a, b)
-- zip_tree Leaf Leaf  =  Leaf
-- zip_tree Leaf (Node left this right)  =  Leaf
-- zip_tree (Node left this right) Leaf  =  Leaf
-- zip_tree (Node left1 this1 right1) (Node left2 this2  right2)  =
--    Node (zip_tree left1 left2) (this1 , this2) (zip_tree right1 right2)

zip_tree : Tree shape a -> Tree shape b -> Tree shape (a, b)
zip_tree Leaf Leaf = Leaf
zip_tree (Node left1 this1 right1) (Node left2 this2 right2) =
    Node (zip_tree left1 left2) (this1, this2) (zip_tree right1 right2)


flip_shape : TreeShape -> TreeShape
flip_shape LeafShape = LeafShape
flip_shape (NodeShape l r) = (NodeShape (flip_shape r) (flip_shape l))

flip_tree : Tree shape a -> Tree (flip_shape shape) a
flip_tree Leaf = Leaf
flip_tree (Node left this right) =
    Node (flip_tree right) this (flip_tree left)


data  PathTo:(targetS:TreeShape) -> (treeS : TreeShape) -> Type  where
    Here  :  PathTo targetS targetS
    Left  :  (path : PathTo targetS l) -> PathTo targetS (NodeShape l r)
    Right :  (path : PathTo targetS r) -> PathTo targetS (NodeShape l r)

replace_node : a -> PathTo (NodeShape l r) sh -> Tree sh a -> Tree sh a
replace_node new Here (Node left _ right) = Node left new right
replace_node new (Left path) (Node left this right) =Node (replace_node new path left) this right
replace_node new (Right path) (Node left this right) = Node left this (replace_node new path right)


shape_graft  :  (branchS : TreeShape) -> (stalkS : TreeShape) ->
    (path : PathTo LeafShape stalkS) -> TreeShape
shape_graft branchS LeafShape Here  =  branchS
shape_graft branchS (NodeShape l r) (Left path)  =
    NodeShape (shape_graft branchS l path) r
shape_graft branchS (NodeShape l r) (Right path)  =
    NodeShape l (shape_graft branchS r path)

tree_graft : (branch : Tree branchS a) -> (stalk : Tree stalkS a) -> (path : PathTo LeafShape stalkS) -> Tree (shape_graft branchS stalkS path) a
tree_graft branch Leaf Here = branch
tree_graft branch (Node left this right) (Left path) =  Node (tree_graft branch left path) this right
tree_graft branch (Node left this right) (Right path) = Node left this (tree_graft branch right path)