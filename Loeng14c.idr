module Loeng14c

import Monaad

-- Aritmeetilised avaldised (lihtsuse mõttes jälle ilma jagamiseta).

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

-- Tahame jälle kirjutada väärtustaja, aga seekord tahame seda lisaks instrumenteerida väärtustatavate operatsioonide arvu lugemisega.

Counter = Int

eval1 : Expr -> (Int,Counter)

eval1 (Num i) = (i , 0)

eval1 (e1 :+: e2) = case eval1 e1 of
                      (v1,c1) => case eval1 e2 of
                                   (v2,c2) => (v1 + v2 , c1 + c2 + 1)

eval1 (e1 :-: e2) = case eval1 e1 of
                      (v1,c1) => case eval1 e2 of
                                   (v2,c2) => (v1 - v2 , c1 + c2 + 1)

eval1 (e1 :*: e2) = case eval1 e1 of
                      (v1,c1) => case eval1 e2 of
                                   (v2,c2) => (v1 * v2 , c1 + c2 + 1)

{-

Avaldised väärtustuvad nagu oodatud

  Loeng14c> eval1 exp1                -- => (3, 1)
  Loeng14c> eval1 exp2                -- => (6, 2)

Samas jällegi palju koodi duplitseerimist. Kas saame ka siin sarnased mustrid abstraheerida?

-}

-- Korduvad mustrid: väärtuste tagastamine, väärtustamiste järjestikku komponeerimine, operatsioonide arvu lugemine.

Ctr' : Type -> Type
Ctr' a = (a , Counter)

cReturn' : a -> Ctr' a
cReturn' x = (x , 0)

cBind' : Ctr' a -> (a -> Ctr' b) -> Ctr' b
cBind' comp f = case comp of
                  (x,c1) => case f x of
                              (y,c2) => (y, c1 + c2)

count' : Ctr' ()
count' = ((), 1)

-- Teine katse väärtustaja kirjutamiseks, abstraheerides koodimustrid.

eval2 : Expr -> Ctr' Int

eval2 (Num i) = cReturn' i

eval2 (e1 :+: e2) = eval2 e1 `cBind'` \ v1 => 
                    eval2 e2 `cBind'` \ v2 =>
                    count' `cBind'` \ () =>
                    cReturn' (v1 + v2)

eval2 (e1 :-: e2) = eval2 e1 `cBind'` \ v1 => 
                    eval2 e2 `cBind'` \ v2 =>
                    count' `cBind'` \ () =>
                    cReturn' (v1 - v2)

eval2 (e1 :*: e2) = eval2 e1 `cBind'` \ v1 => 
                    eval2 e2 `cBind'` \ v2 =>
                    count' `cBind'` \ () =>
                    cReturn' (v1 * v2)

{-

Avaldised väärtustuvad nagu oodatud

  Loeng14c> eval2 exp1                -- => (3, 1)
  Loeng14c> eval2 exp2                -- => (6, 2)

-}

-- Näitame, et Ctr' on ka monaad. 

{- 

Et Idris rahul oleks, peame Ctr' tüübile panema ümber triviaalse ühe konstruktoriga andmetüübi kihi.

-}

data Ctr a = MkCtr (Ctr' a)

cReturn : a -> Ctr a
cReturn x = MkCtr (x , 0)

cBind : Ctr a -> (a -> Ctr b) -> Ctr b
cBind comp f = case comp of
                 MkCtr (x,c1) => case (f x) of
                                   MkCtr (y,c2) => MkCtr (y , c1 + c2)

count : Ctr ()
count = MkCtr ((), 1)

Monaad Ctr where
  return = cReturn
  bind   = cBind

-- Kuna Ctr on Monaad ja seega ka Monad, siis saame väärtustaja ka siin kirjutada do-notatsiooniga.

eval3 : Expr -> Ctr Int

eval3 (Num i) = return i

eval3 (e1 :+: e2) = do
  v1 <- eval3 e1
  v2 <- eval3 e2
  count
  return (v1 + v2)

eval3 (e1 :-: e2) = do
  v1 <- eval3 e1
  v2 <- eval3 e2
  count
  return (v1 - v2)

eval3 (e1 :*: e2) = do
  v1 <- eval3 e1
  v2 <- eval3 e2
  count
  return (v1 * v2)

{-

Avaldised väärtustuvad nagu oodatud

  Loeng14c> eval3 exp1                -- => MkCtr (3, 1)
  Loeng14c> eval3 exp2                -- => MkCtr (6, 2)

-}

-- Ja nagu veatöötlusegi puhul, saame siin ka !-notatsiooni kasutada.

{-

Pange aga tähele, et kui eval4 programmis !-notatsiooni kasutamised do-notatsiooniga 
return-ide seest välja tõsta, siis tehakse count süntaktiliselt enne eval4 e1 ja 
eval4 e2 väljakutseid. Seega on eval4 süntaktiliselt erinev eval3 programmist. Samas, 
kuna Int arvute liitmine on kommutatiivne, siis semantiliselt siin vahet pole, mis 
järjekorras operatsioonid ja funktsioonide väljakutsed tehakse. See erinevus hakkab 
rolli mängima kui Ctr monaadi kombineerida teiste monaadidega, nagu näiteks praktikums. 

-}

eval4 : Expr -> Ctr Int

eval4 (Num i) = return i

eval4 (e1 :+: e2) = do
  count
  return (!(eval4 e1) + !(eval4 e2))

eval4 (e1 :-: e2) = do
  count
  return (!(eval4 e1) - !(eval4 e2))

eval4 (e1 :*: e2) = do
  count
  return (!(eval4 e1) * !(eval4 e2))

{-

Avaldised väärtustuvad nagu oodatud

  Loeng14c> eval4 exp1                -- => MkCtr (3, 1)
  Loeng14c> eval4 exp2                -- => MkCtr (6, 2)

-}

-- Lisanäide: Üldisem kirjutajamonaad suvalise monoidi m jaoks.

data Writer m a = MkWriter (a , m)       -- m on kirjutatavate väärtuste monaadi, a on tagastatavate väärtuste tüüp

wReturn : Monoid m => a -> Writer m a
wReturn x = MkWriter (x , neutral)

wBind : Monoid m => Writer m a -> (a -> Writer m b) -> Writer m b
wBind comp f = case comp of
                 MkWriter (x,m) => case f x of
                                     MkWriter (y,n) => MkWriter (y,m <+> n)

Monoid m => Monaad (Writer m) where
  return = wReturn
  bind   = wBind

write : Monoid m => m -> Writer m ()    -- + 1 asemel on meil nüüd üldisema väärtusega z : m kirjutamise operatsioon
write z = MkWriter ((),z)