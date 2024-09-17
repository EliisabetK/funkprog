module K2

fst : (a, b) -> a
fst (a, b) = a

-- varjame sisseehitatud listi pikkuse arvutuse
%hide Prelude.List.length
 
length : List a -> Int
length [] = 0
length (x :: xs) = 1 + length xs
--K2.length [2, 3, 4] = 1 + length [3,4] = 1 + 1 + length [4] = 2 + length [4] = 2 + 1 + length [] = 3 + 0 = 3

-- varjame sisseehitatud konkateneerimise arvutuse
%hide Prelude.Types.List.(++)
 
infixr 7 ++
(++) : List a -> List a -> List a
(++) [] ys = ys
(++) (x :: xs) ys = x :: (++) xs ys
--K2.(++) [1] [2] = 1 :: (++) [2] = 1 :: (++)[][2] = 1 :: [2]

replicate : Int -> a -> List a
replicate n a = if n > 0 then a :: replicate (n - 1) a else []


take : Int -> List a -> List a
take n [] = []
take 0 (x :: xs) = []
take n (x :: xs) = x :: take (n - 1) xs


sum : List Integer -> Integer
sum [] = 0
sum (x :: xs) = x + sum xs
-- sum [100, 76, 24] = 100 + sum [76, 24] = 100 + 76 + sum [24] = 176 + sum [24] = 176 + 24 = 200


drop : Int -> List a -> List a
drop n [] = []
drop 0 xs = xs
drop n (x :: xs) = drop (n - 1) xs


--peidame sisseehitatud funktsiooni
%hide Prelude.Types.List.reverse
 
reverse : List a -> List a
reverse [] = []
reverse (x :: xs) = reverse xs ++ [x]


esimesed : List (a, b) -> List a
esimesed [] = []
esimesed (x :: xs) =  esimesed xs ++ [fst(x)]


otsi : Integer -> List Integer -> Bool
otsi n xs = ?rhs_otsi


dropLast : List a -> List a
dropLast xs = reverse (drop 1 (reverse xs))


lisa : Int -> Char -> String -> String
lisa i x ys = ?rhs_lisa


lines : String -> List String
lines = ?rhs_lines