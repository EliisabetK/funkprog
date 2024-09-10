module K1
 
-- kirjutage funktsioonid siia

sumInt : Int -> Int
sumInt x = if x == 0 then 0 else sumInt(x-1)+1
-- sumint 3 = sumint(3-1)+3  = sumint(2) + 3 = sumint(2-1) + 2 + 3 = sumint(1) + 2 + 3 = sumint(0) + 1 + 2 + 3 = 0 + 1 + 2 + 3 = 6


fib : Int -> Int
fib x = if x==0 || x==1 then x else fib(x - 1) + fib(x - 2)
-- fib(3) = fib(3-1) + fib(3-2) = fib(2) + fib(1) = fib(2-1) + fib(2-2) + 1 = fib(1) + 0 + 1 = 1 + 1 = 2


modulo : Int -> Int -> Int
modulo x y =
    let f : Int -> Int
        f n = if n < y then n else f(n-y)
    in f x
-- modulo(5,2) = modulo (5-2) 2 = modulo (3) 2 = modulo (3-2) 2 = modulo (1) 2 = 1


syt : Int -> Int -> Int
syt x y = case y == 0 of
            False => syt y (modulo x y)
            True => x


mc : Int -> Int
mc n = if n < 100 then n - 10 else mc(mc(n+11))


korda : Int -> (Int -> Int) -> Int -> Int
korda 0 f x = x
korda n f x = f (korda (n - 1) f x)
 
inc : Int -> Int
inc x = x + 1

add : Int -> Int -> Int
add x y = korda y inc x

mul : Int -> Int -> Int
mul x y = korda y (add x) 0


aste : Int -> Int -> Int
aste x n = if n == 0 then 1 else x * aste x (n-1)


qaste : Int -> Int -> Int
qaste x n =