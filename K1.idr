module K1
 
-------------------------------1---------------------------------

sumInt : Int -> Int
sumInt x = if x == 0 then 0 else sumInt(x-1)+x
-- sumint 3 = sumint(3-1)+3  = sumint(2) + 3 = sumint(2-1) + 2 + 3 = sumint(1) + 2 + 3 = sumint(0) + 1 + 2 + 3 = 0 + 1 + 2 + 3 = 6

-------------------------------2--------------------------------

fib : Int -> Int
fib x = if x==0 || x==1 then x else fib(x - 1) + fib(x - 2)
-- fib(3) = fib(3-1) + fib(3-2) = fib(2) + fib(1) = fib(2-1) + fib(2-2) + 1 = fib(1) + 0 + 1 = 1 + 1 = 2

------------------------------3--------------------------------

modulo : Int -> Int -> Int
modulo x y =
    let f : Int -> Int
        f n = if n < y then n else f(n-y)
    in f x
-- modulo(5,2) = modulo (5-2) 2 = modulo (3) 2 = modulo (3-2) 2 = modulo (1) 2 = 1

-----------------------------4----------------------------------

syt : Int -> Int -> Int
syt x y = case y == 0 of
            False => syt y (modulo x y)
            True => x
-- syt 12 8 = syt 8 modulo 12 8 = syt 8 4 = syt 4 modulo 8 4 = syt 4 0 = 4

------------------------------5--------------------------------

mc : Int -> Int
mc n = if n > 100 then n - 10 else mc(mc(n+11))

------------------------------6--------------------------------

hanoi : Int -> Int
hanoi n = if n == 1 then 1 else 2*hanoi(n-1)+1

------------------------------7--------------------------------

ack : Int -> Int -> Int
ack m n = if m == 0 then n + 1 
          else if m > 0 && n == 0 then ack (m - 1) 1 
          else ack (m - 1) (ack m (n - 1))

------------------------------8--------------------------------

korda : Int -> (Int -> Int) -> Int -> Int
korda 0 f x = x
korda n f x = f (korda (n - 1) f x)
 
inc : Int -> Int
inc x = x + 1

add : Int -> Int -> Int
add x y = korda y inc x

mul : Int -> Int -> Int
mul x y = korda y (add x) 0

------------------------------9-------------------------------

aste : Int -> Int -> Int
aste x n = if n == 0 then 1 else x * aste x (n-1)

-----------------------------10--------------------------------

p : Int -> Int -> Int
p n k = if k == 0 then 1 else n * p (n - 1) (k - 1)

-----------------------------11--------------------------------

c : Int -> Int -> Int
c n 0 = 1
c n k = if k == n then 1 else if 1 <= k && k <= n-1 then c (n-1) (k-1) + c (n-1) k else 0

-----------------------------12--------------------------------

qaste : Int -> Int -> Int
qaste x 0 = 1 
qaste x n = 
  let y = qaste x (n `div` 2) in
  if n `mod` 2 == 0  then y * y else x * y * y

-----------------------------13--------------------------------

ndiv : Int -> Int -> Int
ndiv x y = f x 0 where
  f : Int -> Int -> Int
  f n z = if n < y then z else f (n - y) (z + 1)
