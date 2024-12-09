module Loeng14b

import Monaad

-- Aritmeetilised avaldised (koos jagamiseta).

infixl 10 :+:, :-: 
infixl 11 :*:, :/:

data Expr = Num Int
          | (:+:) Expr Expr
          | (:-:) Expr Expr
          | (:*:) Expr Expr
          | (:/:) Expr Expr

exp1 : Expr
exp1 = Num 1 :+: Num 2

exp2 : Expr
exp2 = Num 2 :*: (Num 4 :-: Num 1)

exp3 : Expr
exp3 = Num 4 :*: Num 3 :/: Num 2

exp4 : Expr
exp4 = Num 4 :*: Num 3 :/: (Num 2 :-: Num 2)

-- Esimene katse väärtustaja defineerimiseks.

eval1 : Expr -> Int
eval1 (Num i) = i
eval1 (e1 :+: e2) = eval1 e1 + eval1 e2
eval1 (e1 :-: e2) = eval1 e1 - eval1 e2
eval1 (e1 :*: e2) = eval1 e1 * eval1 e2
eval1 (e1 :/: e2) = eval1 e1 `div` eval1 e2

{-

Esimesed kolm avaldist väärtustuvad nagu oodatud

  Loeng14b> eval1 exp1              -- => 3
  Loeng14b> eval1 exp2              -- => 6
  Loeng14b> eval1 exp3              -- => 6

Neljanda avaldise väärtustamisel tekib viga (nulliga jagamise tõttu) 

  Loeng14b> eval1 exp4              -- => let False = True in prim__div_Int 12 0

kuna `div` definitsioon Integral Int tüübiklassi jaoks on antud järgmiselt

  Integral Int where
    div x y
        = case y == 0 of
               False => prim__div_Int x y

Siin puudub aga edasise veatöötluse võimalus ning ka programmi väljund ei kirjelda 
selge veateatega, et viga on tekkinud ning milles see täpsemalt seisneb. Tüpsemalt, 
tüüp eval1 : Expr -> Int ei väljenda, et väärtustamisel võivad vead/erandid tekkida.

-}

-- Teine katse väärtustaja defineerimiseks. Seekord erandeid ja hiljem ka veatöötlust toetavalt.

-- Esmalt osaliste väärtuste tüüp. 

{-

Option tüüp on somorfne standardteegis oleva Maybe tüübiga, aga selles loengus ja 
praktikumis on mugavam uus tüüp defineerida, kuna muidu tekivad meil hiljem probleemid
sellega, et Maybe tüüp saaks olema näide Monad tüübiklassist rohkem kui ühel viisil.

-}

data Option a = None
              | Some a

eval2 : Expr -> Option Int
eval2 (Num i) = Some i
eval2 (e1 :+: e2) = case eval2 e1 of
                      None => None
                      Some v1 => case eval2 e2 of
                                   None => None
                                   Some v2 => Some (v1 + v2)
eval2 (e1 :-: e2) = case eval2 e1 of
                      None => None
                      Some v1 => case eval2 e2 of
                                   None => None
                                   Some v2 => Some (v1 - v2)
eval2 (e1 :*: e2) = case eval2 e1 of
                      None => None
                      Some v1 => case eval2 e2 of
                                   None => None
                                   Some v2 => Some (v1 * v2)
eval2 (e1 :/: e2) = case eval2 e1 of
                      None => None
                      Some v1 => case eval2 e2 of
                                   None => None
                                   Some v2 => 
                                     if v2 == 0 then None else Some (v1 `div` v2)

{-

Esimesed kolm avaldist väärtustuvad nagu oodatud

  Loeng14b> eval2 exp1              -- => Some 3
  Loeng14b> eval2 exp2              -- => Some 6
  Loeng14b> eval2 exp3              -- => Some 6

Neljanda avaldise väärtustamisel tekib jälle veaolukord, aga sedapuhku 
korraliku erandi tõstatamisega (ning võimalusega seda hiljem töödelda)

  Loeng14b> eval2 exp4              -- => None

Samal ajal sisaldab aga eval2 palju koodi duplitseerimist. Kas saame korduvaid mustreid abstraheerida?

-}

-- Korduvad mustrid: väärtuste tagastamine, väärtustamiste järjestikku käivitamine, erandite tõstatamine.

oReturn : a -> Option a
oReturn x = Some x

oBind : Option a -> (a -> Option b) -> Option b
oBind comp f = case comp of
                 None => None
                 Some x => f x

oThrow : Option a
oThrow = None

-- Kolmas katse väärtustaja defineerimiseks, sedakorda abstraheerides koodimustreid.

eval3 : Expr -> Option Int

eval3 (Num i) = oReturn i

eval3 (e1 :+: e2) = eval3 e1 `oBind` \ v1 => 
                    eval3 e2 `oBind` \ v2 => 
                    oReturn (v1 + v2)

eval3 (e1 :-: e2) = eval3 e1 `oBind` \ v1 => 
                    eval3 e2 `oBind` \ v2 => 
                    oReturn (v1 - v2)

eval3 (e1 :*: e2) = eval3 e1 `oBind` \ v1 => 
                    eval3 e2 `oBind` \ v2 => 
                    oReturn (v1 * v2)

eval3 (e1 :/: e2) = eval3 e1 `oBind` \ v1 => 
                    eval3 e2 `oBind` \ v2 => 
                    if v2 == 0 then oThrow else oReturn (v1 `div` v2)

-- Option tüüp on üks näide Monaad tüübiklassist ning seega ka Monad tüübiklassist.

Monaad Option where
  return = oReturn
  bind   = oBind

throw : Option a
throw = oThrow

tryCatch : Option a -> Option a -> Option a
tryCatch comp excHandler = case comp of 
                             None   => excHandler
                             Some x => return x

{-

Kui rakendame veatöötlust varasemalt erandit tekitanud programmile saame oodatult

  Loeng14b> tryCatch (eval2 exp4) (return 42)        -- => Some 42

-}

-- Kuna Option on Monad, saame väärtustaja kirja panna veelgi selgemalt do-notatsiooniga.

eval4 : Expr -> Option Int

eval4 (Num i) = return i

eval4 (e1 :+: e2) = do
  v1 <- eval4 e1
  v2 <- eval4 e2
  return (v1 + v2)

eval4 (e1 :-: e2) = do
  v1 <- eval4 e1
  v2 <- eval4 e2
  return (v1 - v2)

eval4 (e1 :*: e2) = do
  v1 <- eval4 e1
  v2 <- eval4 e2
  return (v1 * v2)

eval4 (e1 :/: e2) = do
  v1 <- eval4 e1
  v2 <- eval4 e2
  if v2 == 0 then throw else return (v1 `div` v2)

-- Kuna Option on Monad, saame väärtustaja kirja panna veelgi lühemalt !-notatsiooniga.

eval5 : Expr -> Option Int

eval5 (Num i) = return i

eval5 (e1 :+: e2) =
  return (!(eval5 e1) + !(eval5 e2))

eval5 (e1 :-: e2) = 
  return (!(eval5 e1) - !(eval5 e2))

eval5 (e1 :*: e2) = 
  return (!(eval5 e1) * !(eval5 e2))

eval5 (e1 :/: e2) = 
  if !(eval5 e2) == 0 then throw else return (!(eval5 e1) `div` !(eval5 e2))

-- Lisanäide: Rohkem kui ühe erandiga erandite ja veatöötluse monaad.

{-

Either tüüp on defineeritud standardteegis järgmise induktiivse tüübina

  data Either : (a : Type) -> (b : Type) -> Type where
    Left  : forall a, b. (x : a) -> Either a b
    Right : forall a, b. (x : b) -> Either a b
  
Either a b esitab tüüpide a ja b lõikumatut ühendit (ingl disjoint union).

-}

data Exc e a = MkExc (Either e a)      -- e on erandite tüüp, a on arvutuse tagastustüüp

eReturn : a -> Exc e a
eReturn x = MkExc (Right x)

eBind : Exc e a -> (a -> Exc e b) -> Exc e b
eBind comp f = case comp of
                 MkExc (Left e)  => MkExc (Left e)
                 MkExc (Right x) => f x

Monaad (Exc e) where
  return = eReturn
  bind   = eBind

eThrow : e -> Exc e a
eThrow e = MkExc (Left e)

eTryCatch : Exc e a -> (e -> Exc e' a) -> Exc e' a       -- veatöötluse tulemusel võime tõstatada ka teistsuguseid erandeid e'
eTryCatch comp excHandler = case comp of
                              MkExc (Left e)  => excHandler e
                              MkExc (Right x) => return x