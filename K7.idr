module K7
import Data.List

--1. add 1 1 = (λm n.λf x.m f(n f x)) 1 1 = λf x.(1)f((1)f x) = λf x.(λf x.f x) f ((λf x.f x) f x) =  λf x.(λx.fx) ((λf x.fx) f x) = λf x.(λx.fx)(λx.f x) x = λf x.(λx.f x) fx =  λf x.f(f(x)) = 2
--2. add 0 0 = (λm n.λf x.m f(n f x)) 1 1 =  λf x.(0) f ((0)f x) = λf x.(λf x. x) f ((λf x.x) f x) =  λf x.(λx. x)((λf x.x) f x) =   λf x.(λx. x) λx.x) x = λf x.(λx. x) x =  λf x. x = 0
--3. mul 2 0 = (λm n.λf x.m(nf)x) 2 0 = λf x.2((0) f)x = λf x.2((λf x.x)f)x = λf x.2(λx.x)x = λf x.(λf x.f(fx))(λx.x)x = λf x.(λx.x)x = λf x.x = 0
--4. mul 2 3 = (λm n.λf x.m (n f) x) 2 3 = λf x. 2 (λx. f(f(fx))) x = λf x. (λf x. f(fx)) (λx. f(f(fx))) x = λf x. (λx. f(f(fx)))((λx. f(f(fx)))x) = λf x. f(f(f((λx. f(f(fx)))x))) = λf x. f(f(f(f(f(fx))))) = 6
--5. exp 2 2 = ( λm n. λf x. n m f x) 2 2 = λf x. 2 2 f x = λf x. 2 (λf x. f(f(x))) f x = λf x.(λf x. f(f(x))) (λf x. f(f(x))) f x = 
--= λfx.(λf.f(f(λfx. ffx))) (λfx. ffx) f x =  λf x.(λf x. f f x )((λf x . f f x)f) x =  λf x.(λf x. f f x )(λx . f f x) x =
--=λf x.(λf x. f f x )(λx. f f x) x = λf x.(λf x. f f x )f f x =  λf x.(λf x. f f x )f f x
--= λf x.(λx. λx. f f x )(((λx. f f x)x))x =  λf x.(λx. λx. f f x )(((λx. f f x)x))x 
--=λf x.(λx. (f f f f x) x) = λf x. f f f f x = 4
--6. exp 2 0 = (λm n. λf x. n m f x) 2 0 = λf x. 0 2 f x = λf x. (λf x. x) (λf x. f(f(x))) f x = λf x. (λx. x) f x = λf x. f x = 1

 
data Tree k v = E | T (Tree k v) k v (Tree k v)
 
test124 : Tree Int String
test124 = T (T E 1 "\252ks" (T E 2 "kaks" E)) 4 "neli" E
 
treeToList : Tree k v -> List (k,v)
treeToList E = []
treeToList (T x y z w) = (y,z) :: treeToList x ++ treeToList w

(Eq k, Eq v) => Eq (Tree k v) where
  (==) E E = True
  (==) E (T x y z w) = False
  (==) (T x y z w) E = False
  (==) (T x y z w) (T x' y' z' w') = x == x' && y == y' && z == z' && w == w'

lookup : Ord k => k -> Tree k v -> Maybe v
lookup k E = Nothing
lookup k ( T v key value p) = case compare k key of
                                    LT => lookup k v
                                    EQ => Just value
                                    GT => lookup k p

insert : Ord k => k -> v -> Tree k v -> Tree k v
insert k v E = T E k v E
insert k v (T vasak key value parem) = case compare k key of
                                            LT => T (insert k v vasak) key value parem
                                            EQ => T vasak key v parem
                                            GT => T vasak key value (insert k v parem)

fold : Ord k => (k -> v -> a -> a) -> a -> Tree k v -> a
fold f acc E = acc
fold f acc (T vasak k v parem) = 
    let acc' = fold f acc vasak
        acc'' = f k v acc'
    in fold f acc'' parem


insertAll : Ord k => List (k,v) -> Tree k v -> Tree k v
insertAll [] tree = tree
insertAll ((a, b) :: xs) tree = insertAll xs (insert a b tree)


combine : Ord k => Tree k v -> Tree k v -> Tree k v
combine tree E = tree
combine tree1 tree2 = fold insert tree2 tree1
 

remove : Ord k => k -> Tree k v -> Tree k v
remove x E = E
remove x (T vasak key value parem) = case compare x key of
                                          LT => T (remove x vasak) key value parem
                                          GT => T vasak key value (remove x parem)
                                          EQ => combine vasak parem


union : Ord k => Tree k v -> Tree k v -> Tree k v
union E tree = tree
union tree E = tree
union E tree = tree
union tree1 tree2 = fold (\k, v, acc => insert k v acc) tree1 tree2


Set : Type -> Type
Set a = Tree a ()
 
empty : Set a
empty = E
 
setToList : Ord a => Set a -> List a
setToList = fold (\x,(),xs=>x::xs) []

add : Ord a => a -> Set a -> Set a
add x E = T E x () E
add x (T vasak key _ parem) = case compare x key of
                                  LT => T (add x vasak) key () parem
                                  GT => T vasak key () (add x parem)
                                  EQ => T vasak key () parem -- kui element on olemas juba ss ei tee midagi


delete : Ord a => Set a -> Set a -> Set a
delete set1 set2 = foldl (\acc, x => remove x acc) set1 (setToList set2)


intersect : Ord a => Set a -> Set a -> Set a
intersect E x = E
intersect x E = E
intersect (T vasak k x parem) set2 = if not (lookup k set2 == Nothing) then T (intersect vasak set2) k () (intersect parem set2)
                                      else combine (intersect vasak set2) (intersect parem set2)



