module K3
import Data.String

---1. Leia termidest vabad muutujad!
---FV(λf. (λg. g x) (λx. f x)) = x
---FV((λx. (λg. g x)) x) = x
---FV(λg. (λf. f x) (λx. g x)) = x
---FV((λy. y) (λf x. y x) (λx. g x)) = y, g
---FV(z λh x. (λf. h f) x) = z


--- 2. Leia substitutsiooni tulemus!
---(λf. f y(λx.x))[y→λx y. f x] = ( (λf. f (λx y. f x) (λx. x)) )
---(λx. f (x x))(λx. f (x x))[f→λx y. y] = ( (λx. (λx y. y) (x x))(λx. (λx y. y) (x x)) )
---(λx g y.x g y)[g→x g y] = ( λx (x g y) y. x (x g y) y )


mod7 : List Int
mod7 = [ x | x<-[0..1000], mod x 7 == 0]


count : Char -> String -> Nat
count c s = length [ x | x <- unpack s, x == c]


concat' : List (List a) -> List a
concat' xss = ?rhs_concat


factors : Int -> List Int
factors n = [ x | x <- [1..n], mod n x == 0]


primes : Int -> List Int
primes n = ?rhs_primes


zip' : List a -> List b -> List (a,b)
zip' = ?rhs_zip


pairs : List a -> List (a,a)
pairs xs = ?rhs_pairs


and' : List Bool -> Bool
and' = ?rhs_and


sorted  : List Int -> Bool
sorted = ?rhs_sorted


pyths : Int -> List (Int,Int,Int)
pyths n = ?rhs_pyths


perfects : Int -> List Int
perfects n = [ x | x <- [1..n], (sum(factors x) - x) == x]