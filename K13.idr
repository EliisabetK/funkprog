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

------------------------------------Ülesanne 1---------------------------------------------

data OptCtr a = MkOptCtr (Option (a,Counter))
 
Eq a => Eq (OptCtr a) where
  (MkOptCtr None)         == (MkOptCtr None)          = True
  (MkOptCtr None)         == (MkOptCtr (Some (y,c'))) = False
  (MkOptCtr (Some (x,c))) == (MkOptCtr None)          = False
  (MkOptCtr (Some (x,c))) == (MkOptCtr (Some (y,c'))) = x == y && c == c'
 
ocReturn : a -> OptCtr a
ocReturn x = MkOptCtr (Some (x, 0))
 
ocBind : OptCtr a -> (a -> OptCtr b) -> OptCtr b
ocBind (MkOptCtr None) x = MkOptCtr None
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
ocTryCatch (MkOptCtr None) expHandler = expHandler
ocTryCatch edu x = edu

------------------------------------Ülesanne 2---------------------------------------------

data OptCtr' a = MkOptCtr' (Option (Either Counter (a, Counter)))

Eq a => Eq (OptCtr' a) where
  MkOptCtr' None == MkOptCtr' None = True
  MkOptCtr' (Some (Left c1)) == MkOptCtr' (Some (Left c2)) = c1 == c2
  MkOptCtr' (Some (Right (x1, c1))) == MkOptCtr' (Some (Right (x2, c2))) = x1 == x2 && c1 == c2
  c == _ = False

ocReturn' : a -> OptCtr' a
ocReturn' x = MkOptCtr' (Some (Right (x, 0)))

ocBind' : OptCtr' a -> (a -> OptCtr' b) -> OptCtr' b
ocBind' (MkOptCtr' None) _ = MkOptCtr' None
ocBind' (MkOptCtr' (Some (Left c))) _ = MkOptCtr' (Some (Left c))
ocBind' (MkOptCtr' (Some (Right (x, c1)))) f =
                                              case f x of
                                                MkOptCtr' None => MkOptCtr' None
                                                MkOptCtr' (Some (Left c2)) => MkOptCtr' (Some (Left (c1 + c2)))
                                                MkOptCtr' (Some (Right (y, c2))) => MkOptCtr' (Some (Right (y, c1 + c2)))

Monaad OptCtr' where
  return = ocReturn'
  bind = ocBind'

ocCount' : OptCtr' ()
ocCount' = MkOptCtr' (Some (Right ((), 1)))

ocThrow' : OptCtr' a
ocThrow' = MkOptCtr' (Some (Left 0))

ocTryCatch' : OptCtr' a -> OptCtr' a -> OptCtr' a
ocTryCatch' (MkOptCtr' None) expHandler = expHandler
ocTryCatch' (MkOptCtr' (Some (Left c1))) (MkOptCtr' (Some (Right (x, c2)))) = MkOptCtr' (Some (Right (x, c1 + c2)))
ocTryCatch' (MkOptCtr' (Some (Left c1))) (MkOptCtr' (Some (Left c2))) = MkOptCtr' (Some (Left (c1 + c2)))
ocTryCatch' y x = y

---------------------------------------Ülesanne 3--------------------------------------------------------

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

interface Monaad m => CountThrowTryCatchMonaad m where
                                                    count : m ()
                                                    throw : m a
                                                    tryCatch : m a -> m a -> m a
                                                  
CountThrowTryCatchMonaad OptCtr where
                                    count = ocCount
                                    throw = ocThrow
                                    tryCatch = ocTryCatch
 
CountThrowTryCatchMonaad OptCtr' where
                                    count = ocCount'
                                    throw = ocThrow'
                                    tryCatch = ocTryCatch'

eval : CountThrowTryCatchMonaad m => Expr -> m Int
eval (Num n) = return n
eval (e1 :+: e2) = do
                    v1 <- eval e1
                    v2 <- eval e2
                    count
                    return (v1 + v2)
eval (e1 :-: e2) = do
                    v1 <- eval e1
                    v2 <- eval e2
                    count
                    return (v1 - v2)
eval (e1 :*: e2) = do
                    v1 <- eval e1
                    v2 <- eval e2
                    count
                    return (v1 * v2)
eval (e1 :/: e2) = do
                    v1 <- eval e1
                    v2 <- eval e2
                    if v2 == 0 then throw else do
                                                let vastus = v1 `div` v2
                                                count
                                                return vastus

returnValue : OptCtr a -> Option a
returnValue (MkOptCtr None) = None
returnValue (MkOptCtr (Some (x, c))) = Some x

numberOfExps : OptCtr a -> Option Counter
numberOfExps (MkOptCtr None) = None
numberOfExps (MkOptCtr (Some (x, c))) = Some c

returnValue' : OptCtr' a -> Option a
returnValue' (MkOptCtr' None) = None
returnValue' (MkOptCtr' (Some (Left c))) = None
returnValue' (MkOptCtr' (Some (Right (x, c)))) = Some x

numberOfExps' : OptCtr' a -> Counter
numberOfExps' (MkOptCtr' None) = 0
numberOfExps' (MkOptCtr' (Some (Left c))) = c
numberOfExps' (MkOptCtr' (Some (Right (x, c)))) = c

----------------------------------Ülesanne 4--------------------------------------

data OptionT : (Type -> Type) -> Type -> Type where
  MkOptionT : m (Option a) -> OptionT m a
 
omReturn : Monaad m => a -> OptionT m a
omReturn x = MkOptionT (return (Some x))

omBind : Monaad m => OptionT m a -> (a -> OptionT m b) -> OptionT m b
omBind (MkOptionT ma) f = MkOptionT $ do
  opt <- ma
  case opt of
            None => return None
            Some x => let (MkOptionT mb) = f x in mb
 
Monaad m => Monaad (OptionT m) where
  return = omReturn
  bind   = omBind

omLift : Monaad m => m a -> OptionT m a
omLift ma = MkOptionT (do
                        a <- ma
                        return (Some a))

omThrow : Monaad m => OptionT m a
omThrow = MkOptionT (return None)

Monaad List where
  return = \ x => [x]
  bind   = listBind

choose : List (Option a) -> OptionT List a
choose xs = MkOptionT xs

--------------------------------------Ülesanne 5----------------------------------------

data Vars = X | Y | Z
 
Eq Vars where
  X == X = True
  X == _ = False
  Y == Y = True
  Y == _ = False
  Z == Z = True
  Z == _ = False
 
State : Type
State = Vars -> Int
 
lookup : Vars -> State -> Int
lookup x s = s x
 
update : Vars -> Int -> State -> State
update x i s y = if x == y then i else s y
 
St' : Type -> Type
St' a = State -> (a,State)
 
data St : Type -> Type where
  MkSt : St' a -> St a
 
sReturn : a -> St a
sReturn x = MkSt (\ s => (x,s))
 
sBind : St a -> (a -> St b) -> St b
sBind f g = MkSt (\ s => case f of
                           MkSt f' => case f' s of
                                        (x,s') => case g x of
                                                    MkSt g' => g' s')
 
Monaad St where
  return = sReturn
  bind   = sBind
 
prefix 8 ^!
(^!) : Vars -> St Int
(^!) x = MkSt (\ s => (lookup x s,s))
 
infix 7 ^=
(^=) : Vars -> Int -> St ()
(^=) x i = MkSt (\ s => ((),update x i s))
 
run : St a -> State -> (a,State)
run (MkSt f) s = f s
 
stateToList : State -> List Int
stateToList s = [s X , s Y , s Z]
 
runToList : St a -> State -> (a,List Int)
runToList f s = let (x,s') = run f s in (x, stateToList s')

data Tree =
    Leaf
  | Node Tree Vars Int Tree


sumVars : Tree -> St ()
sumVars Leaf = sReturn ()
sumVars (Node left var value right) = do
                                        sumVars left
                                        current <- (^!) var
                                        (^=) var (current + value)
                                        sumVars right


initialState1 : Vars -> Int
initialState1 X = 0
initialState1 Y = 0
initialState1 Z = 0
 
initialState2 : Vars -> Int
initialState2 X = 3
initialState2 Y = 5
initialState2 Z = 7
 
tree1 : Tree
tree1 = Node (Node (Node Leaf Z 5 Leaf) X 2 (Node Leaf Y 7 Leaf)) Y 1 (Node Leaf Z 9 (Node Leaf X 5 Leaf))
 
tree2 : Tree
tree2 = Node (Node (Node Leaf Y 5 Leaf) X 2 Leaf) Z 1 (Node Leaf X 9 (Node Leaf X 5 Leaf))

--------------------------------------Ülesanne 6----------------------------------------

StateT' : (Type -> Type) -> Type -> Type -> Type
StateT' m s a = s -> m (a,s)
 
data StateT : (Type -> Type) -> Type -> Type -> Type where
  MkStateT : StateT' m s a -> StateT m s a
 
sttReturn : Monaad m => a -> StateT m s a
sttReturn x = MkStateT (\s => return (x, s))

sttBind : Monaad m => StateT m s a -> (a -> StateT m s b) -> StateT m s b
sttBind (MkStateT ma) f = MkStateT $ \s => do
                                            (a, s') <- ma s
                                            let (MkStateT mb) = f a
                                            mb s'
 
Monaad m => Monaad (StateT m s) where
                                    return = sttReturn
                                    bind   = sttBind
 
sttGet : Monaad m => StateT m s s
sttGet = MkStateT (\s => return (s, s))

sttSet : Monaad m => s -> StateT m s ()
sttSet s = MkStateT (\x => return ((), s))

sttLift : Monaad m => m a -> StateT m s a
sttLift ma = MkStateT (\s => do
                              a <- ma
                              return (a, s))

-----------------------------------------Ülesanne 7------------------------------------------

T : Type -> Type
T a = StateT Option State a

prefix 8 ^^!
(^^!) : Vars -> T Int
(^^!) x = do
  s <- sttGet
  return (lookup x s)

infix 7 ^^=
(^^=) : Vars -> Int -> T ()
(^^=) x i = do
  s <- sttGet
  sttSet (update x i s)

tThrow : T a
tThrow = sttLift None

tTryCatch : T a -> T a -> T a
tTryCatch (MkStateT comp) (MkStateT handler) =
  MkStateT $ \state =>
    case comp state of
      None => handler state
      Some result => Some result

tRunToList : T a -> State -> Option (a, List Int)
tRunToList (MkStateT comp) initialState =
  case comp initialState of
    None => None
    Some (result, finalState) => Some (result, stateToList finalState)

-------------------------------------------Ülesanne 8----------------------------------------------

data Tree' =
    Leaf'
  | Node' Tree' (Option Vars) Int Tree'

maxVars : Tree' -> T ()
maxVars Leaf' = return ()
maxVars (Node' left optVar value right) = do
  maxVars left
  case optVar of
              None => tThrow
              Some var => do
                current <- (^^!) var
                (^^=) var (max current value)
  maxVars right 

tree1' : Tree'
tree1' =
  Node'
    (Node' (Node' Leaf' (Some Y) 5 Leaf') None 2 Leaf')
    (Some Z) 1
    (Node' Leaf' (Some X) 9 (Node' Leaf' (Some X) 5 Leaf'))
 
tree2' : Tree'
tree2' =
  Node'
    (Node' (Node' Leaf' (Some Y) 5 Leaf') (Some X) 2 Leaf')
    (Some Z) 1
    (Node' Leaf' (Some X) 9 (Node' Leaf' (Some X) 5 Leaf'))

---------------------------------Ülesanne 9---------------------------------------------

interface Monaad m => SeadustegaMonaad m where
 
  leftUnitLaw      : (x : a)
                  -> (f : a -> m b)
                  --------------------------------
                  -> do {x <- return x; f x} = f x
 
  rightUnitLaw     : (comp : m a)
                   ---------------------------------
                  -> do {x <- comp; return x} = comp
 
  associativityLaw : (comp : m a)
                  -> (f : a -> m b)
                  -> (g : b -> m c)
                  -----------------------------------------------------------------------
                  -> do {y <- (do {x <- comp; f x}); g y} = do {x <- comp; y <- f x; g y}


SeadustegaMonaad Option where
  leftUnitLaw x f = Refl
  rightUnitLaw None = Refl
  rightUnitLaw (Some x) = Refl
  associativityLaw None f g = Refl
  associativityLaw (Some x) f g = Refl

--------------------------------Ülesanne 10----------------------------------------

bindToMu : {m : Type -> Type}
        -> (forall a. a -> m a)
        -> (forall a,b. m a -> (a -> m b) -> m b)
        -> m (m a) -> m a
bindToMu return bind = ?rhs_bindtomu
 
muToBind : Functor m
        => (forall a. a -> m a)
        -> (forall a. m (m a) -> m a)
        -> m a -> (a -> m b) -> m b
muToBind eta mu = ?rhs_mutobind