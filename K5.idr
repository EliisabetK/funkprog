module K5
import Data.List
import Data.Nat


-- Normaaljärjekorras
--(λf. ff)((λx.b)(λx.x)) = (λf.ff) ((λx.b)(λx.x)) = ((λf.ff) b) = bb
--(λf. (λg.f)f)((λx. b)(λx. x)) = (λg.(λx.b) (λx.x))((λx.b) (λx.x)) = (λx.b) (λx.x) = b
--((λx. x)(λx. (λy. x w)r)(λx. y)) = (λx.(λy.x w) r) (λx.y) = (λx.(λa.x w) r) (λx.y) = (λa.(λx.y) w) r = (λx.y) w = y


-- Aplikatiivjärjekorras
--(λf. ff)((λx.b)(λx.x)) = (λf.ff) ((λx.b)(λx.x)) = (λf.ff) (b) = bb
--(λf. (λg.f)f)((λx. b)(λx. x)) = (λf.f)((λx. b)(λx. x)) = (λf.f) b = b
--((λx. x)(λx. (λy. x w)r)(λx. y)) = ((λx. x)(λx. x w)(λx. y)) = (λx. x w)(λx. y) = (λx. y) w = y

----------------------------------------------------------------

data Email = E String String

varmo     : Email
kalmer    : Email
karoliine : Email
varmo     = E "varmo.vene"       "ut.ee"
kalmer    = E "kalmer.apinis"    "ut.ee"
karoliine = E "karoliine.holter" "ut.ee"

Show Email where
  show (E nimi domeen) = nimi ++ "@" ++ domeen

----------------------------------------------------------------

record Kiri where
  constructor MkKiri
  saatja   : Email
  saajad   : List Email
  pealkiri : String
  sisu     : String

testkiri : Kiri
testkiri = MkKiri
  { saatja = E "eliisabet.kaasik" "ut.ee"
  , saajad = [E "heisi.kurig" "ut.ee"]
  , pealkiri = "Tähtis!"
  , sisu = "Tere!!!"
  }

pealkiriSisu : Kiri -> String
pealkiriSisu kiri = kiri.pealkiri ++ ": " ++ kiri.sisu

----------------------------------------------------------------

data Tree a = Leaf | Branch (Tree a) a (Tree a)

Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    Branch v1 väärtus1 p1 == Branch v2 väärtus2 p2 =
    v1 == v2 && väärtus1 == väärtus2 && p1 == p2
    x == y = False

---------------------------------------------------------------

height : Tree a -> Int
height (Leaf) = 0
height (Branch v väärtus p) = 1 + max (height v) (height p)

----------------------------------------------------------------

fold : (a -> b -> a -> a) -> a -> Tree b -> a
fold f x Leaf = x
fold f x (Branch v väärtus p) = f (fold f x v) väärtus (fold f x p)

----------------------------------------------------------------

size : Tree a -> Nat
size (Leaf) = 0
size (Branch v väärtus p) =  1 + size v + size p

----------------------------------------------------------------

heightFold : Tree a -> Int
heightFold = fold (\v, väärtus, p => 1 + max v p) 0

----------------------------------------------------------------

memberOf : Eq a => a -> Tree a -> Bool
memberOf x (Leaf)= False
memberOf x (Branch v väärtus p) = x == väärtus || memberOf x v || memberOf x p

----------------------------------------------------------------

balanced : Tree a -> Bool
balanced Leaf = True
balanced (Branch v väärtus p) =
  let h1 = height v
      h2 = height p
      vahe = abs (h1 - h2)
    in vahe <= 1 && balanced v && balanced p

----------------------------------------------------------------

gen : Int -> a -> Tree a
gen 0 väärtus= Leaf
gen 1 väärtus = Branch Leaf väärtus Leaf
gen h väärtus = Branch (gen (h-1) väärtus) väärtus (gen (h-1) väärtus)

----------------------------------------------------------------

tree2list : Tree a -> List a
tree2list Leaf = []
tree2list (Branch v väärtus p) =  tree2list v ++ [väärtus] ++ tree2list p

----------------------------------------------------------------

bOrs : Bool -> Type
bOrs True = String
bOrs False = Int

----------------------------------------------------------------

hundred : (isString : Bool) -> bOrs isString
hundred True = "hundred"
hundred False = 100

----------------------------------------------------------------

reprodT : Nat  -> Type
reprodT 0  = Nat
reprodT (S k)  = Nat -> reprodT k
 
add_all : (k : Nat) -> (acc : Nat) -> reprodT k
add_all Z acc = acc
add_all (S k) acc = \x => add_all k (acc + x)

----------------------------------------------------------------

list2tree : List a -> Tree a
list2tree [] = Leaf
list2tree [x] = Branch Leaf x Leaf
list2tree xs = case splitAt (length xs `div` 2) xs of
    (x, []) => Leaf
    (v, x :: p) => Branch (list2tree v) x (list2tree p)

testPuu : Bool -- testing kas on tasakaalus
testPuu = balanced (list2tree [1,2,3,4,5,6,7,8,9,10]) -- peaks olema True