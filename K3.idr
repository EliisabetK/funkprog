module K3
import Data.String

---1. Leia termidest vabad muutujad! ? teha edasi
---FV(λf. (λg. g x) (λx. f x)) = x
---FV((λx. (λg. g x)) x) = FV λx. (λg. g x) U FV (λx. f x) = (FV (λg. g x) - {x}) U {x} = FV(g x) - {g} + {x} - {x}  U {x} = {x}
---FV(λg. (λf. f x) (λx. g x)) = x
---FV((λy. y) (λf x. y x) (λx. g x)) = y, g
---FV(z λh x. (λf. h f) x) = z


--- 2. Leia substitutsiooni tulemus!
---(λf. f y(λx.x))[y→λx y. f x] = ((λf. f (λx y. f x) (λx. x)))
---(λx. f (x x))(λx. f (x x))[f→λx y. y] = λx.(λx y.y)(x x)(λx.(λx y.y)(x x))
---(λx g y.x g y)[g→x g y] = λx g y.x (x g y) y


mod7 : List Int
mod7 = [ x | x<-[0..1000], mod x 7 == 0]


count : Char -> String -> Nat
count c s = length [ x | x <- unpack s, x == c]


concat' : List (List a) -> List a
concat' xss = [ x | xs <- xss, x <- xs]


factors : Int -> List Int
factors n = [ x | x <- [1..n], mod n x == 0]


isPrime : Int -> Bool
isPrime n = factors n == [1, n]

primes : Int -> List Int
primes n = [ x | x <- [1..n-1], isPrime x]


zip' : List a -> List b -> List (a,b)
zip' [] xs = []
zip' xs [] = []
zip' (x :: xs) (y::ys) = (x, y) :: zip' xs ys


pairs : List a -> List (a, a)
pairs [] = []
pairs xs = zip' xs (drop 1 xs)


and' : List Bool -> Bool
and' [] =  True
and' (x :: xs) = if x == False then False else and' xs


sorted : List Int -> Bool
sorted xs = and' [x <= y | (x, y) <- pairs xs]


pyths : Int -> List (Int, Int, Int)
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a * a + b * b == c * c]


perfects : Int -> List Int
perfects n = [ x | x <- [1..n], (sum(factors x) - x) == x]


pythsOpt : Int -> List (Int, Int, Int)
pythsOpt n = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [b..n], a * a + b * b == c * c] 
-- Alustab b ja c vaatamist seal kus a ja b vastavalt lõpevad, siis ei pea kontrollima et a < b