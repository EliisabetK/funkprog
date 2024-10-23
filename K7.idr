module K7
import Data.List

--1. add 1 1 = lambda m n. lambda f x. m f  (n f x) 1 1 = 

--3. mul 2 0 = λ f x 2 (0 f) x = λ fx. 2((λ f x.x) f) x = λ f x. 2((λx.x)) x = λ f x. (λ x. ( λx.x λx.x) x) x)) = 
--= λ f x. (λ x. (λx.x) x)) = λ f x. (λ x.x) x) =  λ f x. x = 0

--5. exp 2 2 = ( λm n. λf x. n m f x) 2 2 = λf x. 2 2 f x = λf x. 2 (λf x. f(f(x))) f x = λf x.(λf x. f(f(x))) (λf x. f(f(x))) f x = 
--= λfx.(λf.f(f(λfx. ffx))) (λfx. ffx) f x =  λf x.(λf x. f f x )((λf x . f f x)f) x =  λf x.(λf x. f f x )(λx . f f x) x =
--=λf x.(λf x. f f x )(λx. f f x) x = λf x.(λf x. f f x )f f x =  λf x.(λf x. f f x )f f x
--= λf x.(λx. λx. f f x )(((λx. f f x)x))x =  λf x.(λx. λx. f f x )(((λx. f f x)x))x 
--=λf x.(λx. (f f f f x) x) = λf x. f f f f x
 
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
fold = ?rhs_fold


insertAll : Ord k => List (k,v) -> Tree k v -> Tree k v
insertAll [] tree = tree
insertAll ((a, b) :: xs) tree = insertAll xs (insert a b tree)


