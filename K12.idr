module K12

-- ainult täielikud funktsioonid tagavad turvalisuse
%default total 
 
infixl 10 /\
data (/\) : Type -> Type -> Type where
    ConI : a -> b
           ------
        -> a /\ b
 
-- Väljaviimise reegleid ise defineerima ei pea aga saame nende paikapidavust kontrollida.
conEl : a /\ b
        ------
     ->   a
 
-- Reegel kehtib, kui saame anda definitsiooni.
conEl (ConI x y) = x
 
conEr : a /\ b
        ------
     ->   b
 
conEr (ConI x y) = y


infixl 11 \/
data (\/) : Type -> Type -> Type where
    DisjIl :   a
             ------
          -> a \/ b
 
    DisjIr :   b
             ------
          -> a \/ b
 
disjE : (a \/ b) -> (a -> c) -> (b -> c)
        ----------------------------------
    ->               c

disjE (DisjIl a) f g = f a
disjE (DisjIr b) f g = g b



%hide Not  -- juba std. teegis olemas
 
data Not : Type -> Type where
    NotI :   (a -> Void)
             -----------
          ->    Not a
 
NotE : Not a -> a
       ----------
    ->    Void
 
NotE (NotI f)= f


--data Void : Type where
---- meelega jäetud tühjaks
 
VoidE : Void
        ----
    ->    b
VoidE q impossible



ex1 :  a /\ (b -> c) /\ (a -> b)  ->  c
ex1 (ConI (ConI f g) h) = g (h f)


--(Vihje: kasuta lambdat)
ex2 : a \/ Not a -> (a -> b) \/ (b -> a)
ex2 (DisjIl x) = DisjIr (\_ => x)
ex2 (DisjIr (NotI f)) = DisjIl (\b => VoidE (f b))


data Even : Nat -> Type where
    Even_Zero : --------
                 Even 0
 
    Even_Succ :    Even n
                ------------
              -> Even (2+n)
 
even4 : Even 4
even4 = Even_Succ(Even_Succ Even_Zero)
 
even8 : Even 8
even8 = Even_Succ(Even_Succ(Even_Succ(Even_Succ Even_Zero)))
 
plusEvenEven :  Even n -> Even m
            ------------------
            ->    Even (n+m)

plusEvenEven Even_Zero b = b
plusEvenEven (Even_Succ a) b = Even_Succ (plusEvenEven a b)          
                   
multEvenEven :  Even n -> Even m
               ------------------
             ->    Even (n*m)

multEvenEven Even_Zero b = Even_Zero
multEvenEven (Even_Succ a) b = let prf = multEvenEven a b  in plusEvenEven b(plusEvenEven b prf)



data Odd : Nat -> Type where
    Odd_one : --------
               Odd 1
 
    Odd_Succ :    Odd n
                -----------
              -> Odd (2+n)
 
odd7 : Odd 7
odd7 = Odd_Succ(Odd_Succ(Odd_Succ(Odd_one)))
 

evenOdd :   Even n
          ----------
        -> Odd (1+n)

evenOdd Even_Zero = Odd_one
evenOdd (Even_Succ x) = Odd_Succ(evenOdd x)
 

plusOddOdd :  Odd n  ->  Odd m
             -------------------
           ->     Even (n+m)

plusOddOdd Odd_one Odd_one = Even_Succ Even_Zero
plusOddOdd Odd_one (Odd_Succ b) = Even_Succ (plusOddOdd Odd_one b)
plusOddOdd (Odd_Succ a) b = Even_Succ (plusOddOdd a b)


plusEvenOdd :  Even n  ->  Odd m
             -------------------
           ->     Odd (n+m)

plusEvenOdd Even_Zero b = b
plusEvenOdd (Even_Succ a) b = Odd_Succ (plusEvenOdd a b)

plusNullOdd :    Odd m
              ------------
            -> Odd (m+0)

plusNullOdd Odd_one = Odd_one
plusNullOdd (Odd_Succ a)= Odd_Succ (plusNullOdd a)


plusAssoc : (m:Nat) -> (n:Nat) -> (q:Nat)
            ------------------------------
          ->  m + (n + q) = (m + n) + q


plusAssoc Z n q = Refl
plusAssoc (S m) n q =
                    let x = plusAssoc m n q in
                    rewrite x in Refl