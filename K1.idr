module K1
 
-- kirjutage funktsioonid siia

sumInt : Int -> Int
sumInt 0 = 0
sumInt n = n + sumInt (n - 1)

fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

modulo : Int -> Int -> Int
modulo n < y = n
modulo x y = ?rhs_modulo