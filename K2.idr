module K2

fst : (a, b) -> a
fst = ?rhs_fst

-- varjame sisseehitatud listi pikkuse arvutuse
%hide Prelude.List.length
 
length : List a -> Int
length [] = 0
length (_ :: xs) = 1 + length xs
--K2.length [2, 3, 4] = 1 + length [3,4] = 1 + 1 + length [4] = 2 + length [4] = 2 + 1 + length [] = 3 + 0 = 3

-- varjame sisseehitatud konkateneerimise arvutuse
%hide Prelude.Types.List.(++)
 
infixr 7 ++
(++) : List a -> List a -> List a
(++) [] ys = ys
(++) (x :: xs) ys = x :: (++) xs ys


replicate : Int -> a -> List a
replicate = ?rhs_replicate


take : Int -> List a -> List a
take = ?rhs_take


sum : List Integer -> Integer
sum = ?rhs_sum


drop : Int -> List a -> List a
drop = ?rhs_drop


--peidame sisseehitatud funktsiooni
%hide Prelude.Types.List.reverse
 
reverse : List a -> List a
reverse = ?rhs_reverse


esimesed : List (a, b) -> List a
esimesed ps = ?rhs_esimesed


otsi : Integer -> List Integer -> Bool
otsi n xs = ?rhs_otsi


dropLast : List a -> List a
dropLast xs = ?rhs_dropLast


lisa : Int -> Char -> String -> String
lisa i x ys = ?rhs_lisa


lines : String -> List String
lines = ?rhs_lines