module K3
import Data.String

---1. Leia termidest vabad muutujad 
---FV(λf.(λg.gx)(λx.fx))=FV((λg.gx)(λx.fx))−{f}=(FV(λg.gx)∪FV(λx.fx))−{f}=({x}∪{f})−{f} = {x}

---FV((λx.(λg.gx))x)=FV(λx.(λg.gx))∪FV(x)=(FV(λg.gx)−{x})∪{x}={x}−{x}∪{x} = {x}

---FV(λg.(λf.fx)(λx.gx))=FV((λf.fx)(λx.gx))−{g}=(FV(λf.fx)∪FV(λx.gx))−{g}=({x}∪{g})−{g} = {x}

---FV((λy.y)(λfx.yx)(λx.gx))=FV(λy.y)∪FV(λfx.yx)∪FV(λx.gx)=∅ ∪{y}∪{g} = {y,g}

---FV(z λhx.(λf.hf)x)=FV(z)∪FV(λhx.(λf.hf)x)={z}∪(FV(λf.hf)∪FV(x)−{h,x})={z}∪({h}∪{x}−{h,x}) = {z}


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