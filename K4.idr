module K4
import Data.List

filter' : (a -> Bool) -> List a -> List a
filter' tingimus [] = []
filter' tingimus (x :: xs) = if tingimus x then x :: filter' tingimus xs else filter' tingimus xs


nullid1 : List Int -> Int
nullid1 [] = 0
nullid1 (x :: xs) = if x == 0 then 1 + nullid1 xs else nullid1 xs

nullid2 : List Int -> Int
nullid2 xs = foldr g 0 xs
    where g : Int -> Int -> Int
          g x summa = if x == 0 then summa + 1 else summa

nullid3 : List Int -> Int
nullid3 xs = sum (map (\x => if x == 0 then 1 else 0) xs)

nullid4 : List Int -> Nat
nullid4 xs = length(filter' ( == 0) xs)

nullid5 : List Int -> Int
nullid5 xs = length [ x | x <- xs, x == 0]


length' : List a -> Int
length' xs = foldr g 0 xs
    where g : a -> Int -> Int
          g a pikkus = 1 + pikkus


productList : List Int -> Int
productList xs = foldr g 1 xs
    where g : Int -> Int -> Int
          g a korrutis = a * korrutis
--productList [3,2,0] = foldr g 1 [3, 2, 0] = 

