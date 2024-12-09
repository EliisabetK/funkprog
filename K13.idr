module K13

interface Monaad (0 m : Type -> Type) where
  return : a -> m a
  bind   : m a -> (a -> m b) -> m b
 
Monaad m => Functor m where
  map f p = bind p (return . f)                      -- : (a -> b) -> m a -> m b
 
Monaad m => Applicative m where
  pure      = return                                 -- : a -> m a
  (<*>) p q = bind p (\ f => bind q (return . f))    -- : m (a -> b) -> m a -> m b
 
Monaad m => Monad m where
  (>>=) = bind                                       -- : m a -> (a -> m b) -> m b

data Option a =
    None
  | Some a
 
Eq a => Eq (Option a) where
  None   == None   = True
  Some x == None   = False
  None   == Some y = False
  Some x == Some y = x == y
 
oReturn : a -> Option a
oReturn x = Some x
 
oBind : Option a -> (a -> Option b) -> Option b
oBind comp f = case comp of
                 None => None
                 Some x => f x
 
Monaad Option where
  return = oReturn
  bind   = oBind
 
oThrow : Option a
oThrow = None

Counter = Int
 
Ctr' : Type -> Type
Ctr' a = (a , Counter)
 
data Ctr a = MkCtr (Ctr' a)
 
cReturn : a -> Ctr a
cReturn x = MkCtr (x , 0)
 
cBind : Ctr a -> (a -> Ctr b) -> Ctr b
cBind comp f = case comp of
                  MkCtr (x,c1) => case (f x) of
                                    MkCtr (y,c2) => MkCtr (y , c1 + c2)
 
Monaad Ctr where
  return = cReturn
  bind   = cBind
 
cCount : Ctr ()
cCount = MkCtr ((), 1)

---------------------------------------------------------------------------------

data OptCtr a = MkOptCtr (Option (a,Counter))
 
Eq a => Eq (OptCtr a) where
  (MkOptCtr None)         == (MkOptCtr None)          = True
  (MkOptCtr None)         == (MkOptCtr (Some (y,c'))) = False
  (MkOptCtr (Some (x,c))) == (MkOptCtr None)          = False
  (MkOptCtr (Some (x,c))) == (MkOptCtr (Some (y,c'))) = x == y && c == c'
 
ocReturn : a -> OptCtr a
ocReturn x = MkOptCtr (Some (x, 0))
 
ocBind : OptCtr a -> (a -> OptCtr b) -> OptCtr b
ocBind (MkOptCtr None) _ = MkOptCtr None
ocBind (MkOptCtr (Some (x, c1))) f = 
  case f x of
    MkOptCtr None => MkOptCtr None
    MkOptCtr (Some (y, c2)) => MkOptCtr (Some (y, c1 + c2))
 
Monaad OptCtr where
  return = ocReturn
  bind   = ocBind
 
ocCount : OptCtr ()
ocCount = MkOptCtr (Some ((), 1))

ocThrow : OptCtr a
ocThrow = MkOptCtr None
 
ocTryCatch : OptCtr a -> OptCtr a -> OptCtr a
ocTryCatch (MkOptCtr None) handler = handler
ocTryCatch success _ = success

---------------------------------------------------------------------------------

data OptCtr' a = MkOptCtr' (Option (a, Counter))
 
Eq a => Eq (OptCtr' a) where
  (MkOptCtr' None) == (MkOptCtr' None) = True
  (MkOptCtr' None) == (MkOptCtr' (Some (_, _))) = False
  (MkOptCtr' (Some (_, _))) == (MkOptCtr' None) = False
  (MkOptCtr' (Some (x, c))) == (MkOptCtr' (Some (y, c'))) = x == y && c == c'
 
ocReturn' : a -> OptCtr' a
ocReturn' x = MkOptCtr' (Some (x, 0))

ocBind' : OptCtr' a -> (a -> OptCtr' b) -> OptCtr' b
ocBind' (MkOptCtr' None) _ = MkOptCtr' (Some ((), 0)) 
ocBind' (MkOptCtr' (Some (x, c1))) f =
  case f x of
    MkOptCtr' None => MkOptCtr' (Some ((), c1)) 
    MkOptCtr' (Some (y, c2)) => MkOptCtr' (Some (y, c1 + c2))


Monaad OptCtr' where
  return = ocReturn'
  bind   = ocBind'
 
ocCount' : OptCtr' ()
ocCount' = MkOptCtr' (Some ((), 1))

ocThrow' : OptCtr' a
ocThrow' = MkOptCtr' None

ocTryCatch' : OptCtr' a -> OptCtr' a -> OptCtr' a
ocTryCatch' (MkOptCtr' None) handler = handler
ocTryCatch' success _ = success