module Loeng14a

-- Aritmeetilised avaldised (ilma jagamiseta).

infixl 10 :+:, :-: 
infixl 11 :*:

data Expr = Num Int
          | (:+:) Expr Expr
          | (:-:) Expr Expr
          | (:*:) Expr Expr

exp1 : Expr
exp1 = Num 1 :+: Num 2

exp2 : Expr
exp2 = Num 2 :*: (Num 4 :-: Num 1)

-- Puhas, efektivaba aritmeetiliste avaldiste väärtustaja.

eval : Expr -> Int
eval (Num i) = i
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2
eval (e1 :*: e2) = eval e1 * eval e2

{-

Avaldised väärtustuvad nagu oodatud.

  Loeng14a> eval exp1        -- => 3
  Loeng14a> eval exp2        -- => 6

-}