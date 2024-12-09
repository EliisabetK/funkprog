module Loeng14d

import Monaad

{-

Idrise programmides esinevad muutujad ei ole muudetavad ja ülekirjutatavad.

Seetõttu ei saa me järgmisel kujul programmi Idrises esitada (kuid saaksime 
seda teha imperatiivsetes keeltes nagu näiteks Java või C)
    
  int x;
  int y;
  int z;

  ...

  int foo(int i, int j) {  
    x = x + i;
    y = x + j;
    x = 42;
    return z
  }

Probleemiks on see, et Idris nagu paljud teised funktsionaalsed programmeerimiskeeled 
(nt Haskell) on oma käitumiselt puhas, efekti-vaba keel.

Funktsioonid kujul f : Int -> Bool on matemaatilised funktsioonid, mis Int-tüüpi sisendite 
andmisel tagastavad Bool-tüüpi väljundeid ning neil ei ole võimalik taustal mällu kirjutada 
ega seal andmeid hoida. Muudetavate muutujate kasutamiseks oleks meil aga midagi sellist vaja, 
kuna peame meeles pidama muutuja väärtust läbi programmi ning võimaldama seda muuta.

Üks viis muudetavate muutujate modelleerimiseks Idrises (ja teistes sarnastes keeltes) on monaadid. 

Selleks vaatleme allpool funktsioonide 
  
  f : Int -> Bool

asemel funktsioone 

   f : Int -> St Bool
   
kus St on niinimetatud olekumonaad (või siin täpsemalt üks konkreetne näide olekumonaadidest).

NB: Selles näites käistleme me ainult globaalselt deklareeritud muudetavaid muutujaid. Monaadidega
on võimalik esitada ka lokaalselt (funktsiooni sees) deklareeritud muutujaid, aga siis peaksime 
kasutama Vars tüübi (vt all) asemel mõnda loenduvalt lõpmatut tüüpi (nt Nat), et lokaalselt oleks
võimalik muutujaid juurde luua. Sellisel juhul oleks vaja lisaks kasutada ka liideste põhist 
abstraktsiooni, et muutujate konkreetne esitus (näiteks Nat-väärtustena) kasutaja eest ära peita.

-}

-- Esmalt defineerime muudetavate muutujate hulga. Lihtsuse mõttes olgu meil ainult kolm muutujat.

data Vars = X | Y | Z

Eq Vars where
  X == X = True
  X == _ = False
  Y == Y = True
  Y == _ = False
  Z == Z = True
  Z == _ = False

-- Olekud, kus me säilitame muudetavate muutujate väärtusi.

State : Type
State = Vars -> Int

-- Olekute peal on meil ka olekust muutuja väärtuse lugemise ja selle muutmise operatsioonid.

lookup : Vars -> State -> Int
lookup x s = s x

update : Vars -> Int -> State -> State
update x i s y = if x == y then i else s y

-- Olekumonaad, mis kirjeldab olekut kasutavaid arvutusi, mis teisendavad programmi algoleku programmi lõppväärtuseks ja lõppolekuks.

St' : Type -> Type
St' a = State -> (a,State)

data St : Type -> Type where
  MkSt : St' a -> St a

sReturn : a -> St a
sReturn x = MkSt (\ s => (x,s))

sBind' : St' a -> (a -> St' b) -> St' b          -- lihtsam versioon arusaamise jaoks, ilma monaadi tüübiklassi jaoks vajamineva lisakihita
sBind' f g s = case f s of (x,s') => g x s'

sBind : St a -> (a -> St b) -> St b
sBind (MkSt f) g = MkSt (\ s => case f s of
                                  (x,s') => case g x of
                                              MkSt g' => g' s')

Monaad St where
  return = sReturn
  bind   = sBind

-- Muutuja väärtuse lugemine

prefix 8 ^!
(^!) : Vars -> St Int                             -- intuitiivselt: annab muutuja x väärtuse antud programmipunktis
(^!) x = MkSt (\ s => (lookup x s,s))

-- Muutujale väärtuse omistamine

infix 7 ^=
(^=) : Vars -> Int -> St ()                      -- intuitiivselt: seab muutuja x väärtuseks i antud programmipunktis
(^=) x i = MkSt (\ s => ((),update x i s))

-- Näide

{-

Kirjutame järgmise programmi 
  int x;
  int y;
  int z;

  ...

  int foo(int i, int j) {  
    x = x + i;
    y = x + j;
    x = 42;
    return z
  }

Idrise funktsioonina, mille tüübiks saab 

  foo : Int -> Int -> St Int

kus St on ülal defineeritud olekumonaad.

-}

prog1 : Int -> Int -> St Int
prog1 i j = do
  x <- ^! X
  X ^= x + i
  x' <- ^! X
  Y ^= x' + j
  X ^= 42
  ^! Z

-- Kasutades !-notatsiooni saame selle programmi taaskord veelgi lühemalt kirja panna.

prog2 : Int -> Int -> St Int
prog2 i j = do
  X ^= !(^! X) + i
  Y ^= !(^! X) + j
  X ^= 42
  ^! Z

-- Olekumonaadi abil modelleeritud arvutuste jooksutamiseks defineerime run funktsiooni, 
-- mis igale algolekule tagastab programmi vastava lõppväärtuse ja lõppoleku.

run : St a -> State -> (a,State)
run (MkSt f) s = f s

-- Lõppolekute paremaks kuvamiseks defineerime abifunktsiooni, mis tagastab lõppoleku 
-- täisarvude listi kujul.

stateToList : State -> List Int
stateToList s = [s X , s Y , s Z]

runToList : St a -> State -> (a,List Int)
runToList f s = let (x,s') = run f s in (x, stateToList s')

-- Näidisalgolek näidete käivitamiseks

initialState : State
initialState X = 1
initialState Y = 2
initialState Z = 3

{-

Programm arvutab oodatult

  Loeng14d> runToList (prog1 4 7) initialState                    -- => (3, [42, 12, 3])

  Loeng14d> runToList (prog2 4 7) initialState                    -- => (3, [42, 12, 3])

  Loeng14d> runToList (prog2 4 7) (\ _ => 0)                      -- => (0, [42, 11, 0])

-}

-- Lisanäide: Olekumonaad üldise olekutüübi s jaoks.

ST' : Type -> Type -> Type
ST' s a = s -> (a,s)                  -- s on olekute tüüp, a on tagastatavate väärtuste tüüp

data ST : Type -> Type -> Type where
  MkST : ST' s a -> ST s a

stReturn : a -> ST s a
stReturn x = MkST (\ s => (x,s))

stBind : ST s a -> (a -> ST s b) -> ST s b
stBind (MkST f) g = MkST (\ s => case f s of
                                   (x,s') => case g x of
                                               MkST g' => g' s')

Monaad (ST s) where
  return = stReturn
  bind   = stBind

{-

Olekust muutujate lugemise ja kirjutamise operatsioonide jaoks on nüüd kogu olekut 
lugevad ja kirjutavad operatsioonid (kuna me ei tea olekutüübi s kohta midagi täpsemat).

-}

stGet : ST s s
stGet = MkST (\ s => (s , s))

stSet : s -> ST s ()
stSet s = MkST (\ _ => ((),s))
