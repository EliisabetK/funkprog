module Loeng14e

import Monaad

-- Identsusmonaad puhaste funktsioonide esitamiseks.

data Id a = MkId a

iReturn : a -> Id a
iReturn x = MkId x

iBind : Id a -> (a -> Id b) -> Id b
iBind (MkId x') f = f x'

Monaad Id where
  return = iReturn
  bind   = iBind

-- Isomorfism tüüpide Id a ja a vahel.

i : Id a -> a
i (MkId x) = x

j : a -> Id a
j x = MkId x

ij : (x : a) -> i (j x) = x
ij x = Refl

ji : (x : Id a) -> j (i x) = x
ji (MkId x) = Refl

-- Näide, mis kombineerib puhaste programmide erinevaid esitusviise.

pureprog : Int -> Id Int -> Id Int
pureprog x y = 
  return (x * !y + 7)