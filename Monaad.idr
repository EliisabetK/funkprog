module Monaad

-- Monaadi tüübiklass. Defineerime lihtsama esituse mõttes ise.

public export
interface Monaad (0 m : Type -> Type) where
  return : a -> m a
  bind   : m a -> (a -> m b) -> m b

-- Näitame, et ülaldefineeritud monaadi tüübiklass on Idrisesse sisseehitatud tüübiklassi Monad instants.

Monaad m => Functor m where
  map f p = bind p (return . f)                      -- : (a -> b) -> m a -> m b

Monaad m => Applicative m where
  pure      = return                                 -- : a -> m a
  (<*>) p q = bind p (\ f => bind q (return . f))    -- : m (a -> b) -> m a -> m b

public export
Monaad m => Monad m where
  (>>=) = bind                                       -- : m a -> (a -> m b) -> m b

-- Matemaatilise korrektsuse huvides peaks iga monaad rahuldama ka kolme seadust.

public export
interface Monaad m => SeadustegaMonaad m where

  -- triviaalsed väärtuste tagastamised võib ära jätta ning tagastatava väärtuse x
  -- võib otse ülejäänud programmile f sisendiks anda

  leftUnitLaw      : (x : a) 
                  -> (f : a -> m b) 
                  --------------------------------
                  -> do {x <- return x; f x} = f x

  -- triviaalsed väärtuste tagastamised võib ära jätta ka programmide lõpust

  rightUnitLaw     : (comp : m a) 
                   ---------------------------------
                  -> do {x <- comp; return x} = comp

  -- do/bind/>>= notatsiooniga arvutuste järjestikusel käivitamisel võib sulud ära jätta ning
  -- arvutusi käivitatakse alati vasakult paremale (do notatsiooni puhul ülevalt alla) järjekorras

  associativityLaw : (comp : m a)
                  -> (f : a -> m b)
                  -> (g : b -> m c)
                  -----------------------------------------------------------------------
                  -> do {y <- (do {x <- comp; f x}); g y} = do {x <- comp; y <- f x; g y}
