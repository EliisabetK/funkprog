module K4
import Data.List

filter' : (a -> Bool) -> List a -> List a
filter' tingimus [] = []
filter' tingimus xs = foldr(\y, ys => if tingimus y then y :: ys else ys) [] xs

nullid1 : List Int -> Int
nullid1 [] = 0
nullid1 (x :: xs) = if x == 0 then 1 + nullid1 xs else nullid1 xs

nullid2 : List Int -> Int
nullid2 xs = foldr (\y, summa => if y == 0 then summa + 1 else summa) 0 xs

nullid3 : List Int -> Int
nullid3 xs = sum (map (\x => if x == 0 then 1 else 0) xs)

nullid4 : List Int -> Nat
nullid4 xs = length(filter' ( == 0) xs)

nullid5 : List Int -> Int
nullid5 xs = cast (length [ x | x <- xs, x == 0])


length' : List a -> Int
length' xs = foldr(\y, pikkus => 1 + pikkus) 0 xs


productList : List Int -> Int
productList = foldr (*) 1
--productList [3,2,0] = \ 3 (foldr \ [3, 2, 0]) = \3 (\2 foldr \ [0]) = \3 (\2 (\0 foldr \ [])) = \3 \2 \0 1 =
--= \3 \2 0*1 = \3 \2 0 = \3 2*0 = \3 0 = 3*0 = 0


append' : List a -> List a -> List a
append' xs ys = foldr (::) ys xs


isEven : Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n
 
all' : (a -> Bool) -> List a -> Bool
all' f xs = foldr (\y, ys => if not (f y) then False else ys) True xs


reverse' : List a -> List a
reverse' = foldl rev df
  where
    df : List a
    df = []
    rev : List a -> a -> List a
    rev z x = x :: z


eemaldaNullid : List Int -> List Int
eemaldaNullid = foldr rem df
  where
    df : List Int
    df = []
    rem : Int -> List Int -> List Int
    rem x y = if x == 0 then y else x :: y


allEqual : List Int -> Bool
allEqual [] = True
allEqual (x :: xs) = foldr (\y, z => (x == y) && z) True xs


unzip' : List (a, b) -> (List a, List b)
unzip' = foldr f z
  where
    z : (List a, List b)
    z = ([], []) 
    f : (a, b) -> (List a, List b) -> (List a, List b)
    f (a, b) (xs, ys) = (a :: xs, b :: ys)


removeAll1 : Int -> List Int -> List Int
removeAll1 n xs = foldr (\y, z => if (y == n) then z else (y :: z)) [] xs

removeAll2 : Int -> List Int -> List Int
removeAll2 n xs = filter' ( /= n) xs

removeAll3 : Int -> List Int -> List Int
removeAll3 n xs = [ x | x <- xs, n /= x]


any' : (a -> Bool) -> List a -> Bool
any' f xs = foldr (\y, ys => if (f y) then True else ys) False xs


foldl_ : (b -> a -> b) -> b -> List a -> b
foldl_ f a xs = foldr (\x, z => f z x) a (reverse xs)

-- testin fold_ reverse funktsiooniga mis enne oli et näha kas tulemus sama
reverse2 : List a -> List a
reverse2 = foldl_ rev df
  where
    df : List a
    df = []
    rev : List a -> a -> List a
    rev z x = x :: z
