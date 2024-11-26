module K8
import System.Random

t2ring : IO ()
t2ring = do
  num <- randomRIO (the Int32 1, the Int32 6)
  printLn num
                         
------------------------------------------------------------------------

dialoog : IO ()
dialoog = do
  putStrLn "Mis on sinu nimi?"
  xs <- getLine
  putStrLn ("Tere, " ++ xs)

------------------------------------------------------------------------


prindiArvud : List Int32 -> IO ()
prindiArvud [] = pure () 
prindiArvud (x :: xs) = do
    putStrLn (show x)
    prindiArvud xs

------------------------------------------------------------------------

trükiNimekiri : Stream String -> List String -> IO ()
trükiNimekiri (x :: xs) [] = pure () 
trükiNimekiri (x :: xs) (y :: ys) = do
    putStrLn (x ++" " ++ y)
    trükiNimekiri xs ys
 
tärnid : Stream String
tärnid = "*" :: tärnid
 
pannkoogid : List String
pannkoogid = ["3 muna", "30g suhkrut", "100g nisujahu","250g piima", "20g võid", "näpuotsaga soola"]

------------------------------------------------------------------------

abi : Int -> Stream String
abi n = (show n ++ ".") :: abi (n + 1)

arvud : Stream String
arvud = abi 1

------------------------------------------------------------------------

readMaybe : IO (Maybe Int32)
readMaybe = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing
 
loeArv : IO Int32
loeArv = do
  putStrLn "Sisesta arv: "
  maybeArv <- readMaybe
  case maybeArv of
    Just arv => pure arv
    Nothing => do
      putStrLn "Vale arv!"
      loeArv

------------------------------------------------------------------------

summa2 : IO ()
summa2 = do
  x <- loeArv
  y <- loeArv
  putStrLn ("Summa: " ++ show (x + y))

------------------------------------------------------------------------

summaAbi : Int32 -> Int32 -> IO Int32
summaAbi 0 summa = pure summa
summaAbi n summa = do
  x <- loeArv
  summaAbi (n - 1) (summa + x)

summaN1 : IO ()
summaN1 = do
  putStrLn "Mitu arvu liita?"
  n <- loeArv
  summa <- summaAbi n 0
  putStrLn ("Summa: " ++ show summa)


käsud : Int32 -> IO a -> List (IO a)
käsud 0 _ = []
käsud n käsk = käsk :: käsud (n - 1) käsk

summaN2 : IO ()
summaN2 = do
  putStrLn "Mitu liidetavat?"
  n <- loeArv
  num <- sequence (käsud n loeArv)
  let summa = sum num
  putStrLn ("Summa: " ++ show summa)

------------------------------------------------------------------------

m2ngWhile : Int32 -> Int -> IO ()
m2ngWhile num katseid = do
  pakkumine <- loeArv
  if pakkumine < num
    then do
      putStrLn "Minu number on suurem!"
      m2ngWhile num (katseid + 1)
    else if pakkumine > num
      then do
        putStrLn "Minu number on väiksem"
        m2ngWhile num (katseid + 1)
      else
        putStrLn ("Arvasid õigesti! Arvamiseks kulus " ++ show (katseid + 1) ++ " katset.")

m2ng : IO ()
m2ng = do
  num <- randomRIO (the Int32 1, the Int32 100)
  putStrLn "Arva ära minu arv: 1-100!"
  m2ngWhile num 0

-------------------------------------------------------

m2ngWhileR : Int32 -> Int32 -> Int32 -> IO ()
m2ngWhileR low high num = do
  putStrLn ("Arvan arvu " ++ show num)
  putStrLn "Kas arv on 1. suurem, 2. väiksem või 3. sama?"
  pakkumine <- loeArv
  if (low >= high) then putStrLn "Sa teed sohki" else
    case pakkumine of
      1 => do
        uusPakkumine <- randomRIO (the Int32 (num + 1), the Int32 high)
        m2ngWhileR (num + 1) high uusPakkumine
      2 => do
        uusPakkumine <- randomRIO (the Int32 low, the Int32 (num - 1))
        m2ngWhileR low (num -1 ) uusPakkumine
      3 => putStrLn "Mina võitsin"
      _ => do 
        putStrLn "Proovi uuesti." 
        m2ngWhileR low num pakkumine
           

m2ngR : IO ()
m2ngR = do
  num <- randomRIO (the Int32 1, the Int32 100)
  putStrLn "Proovin arvata sinu mõeldud arvu."
  m2ngWhileR 1 100 num