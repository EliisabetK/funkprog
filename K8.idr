module K8


t2ring : IO ()
t2ring = do
  num <- randomRIO (the Int32 1, the Int32 6)
  print num
                         

dialoog : IO ()
dialoog = do
  putStrLn "Mis on sinu nimi?"
  xs <- getLine
  putStrLn ("Tere, " ++ xs)


prindiArvud : List Int32 -> IO ()
prindiArvud [] = pure () 
prindiArvud (x :: xs) = do
    putStrLn (show x)
    prindiArvud xs


trükiNimekiri : Stream String -> List String -> IO ()
trükiNimekiri (x :: xs) [] = pure () 
trükiNimekiri (x :: xs) (y :: ys) = do
    putStrLn (x ++" " ++ y)
    trükiNimekiri xs ys
 
tärnid : Stream String
tärnid = "*" :: tärnid
 
pannkoogid : List String
pannkoogid = ["3 muna", "30g suhkrut", "100g nisujahu","250g piima", "20g võid", "näpuotsaga soola"]


abi : Int -> Stream String
abi n = (show n ++ ".") :: abi (n + 1)

arvud : Stream String
arvud = abi 1



readMaybe : IO (Maybe Int32)
readMaybe = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing
 
loeArv : IO Int32
loeArv = ?rhs_loeArv


summa2 : IO ()
summa2 = ?rhs_summa2


summaN1 : IO ()
summaN1 = ?rhs_summaN1
 
summaN2 : IO ()
summaN2 = ?rhs_summaN2


m2ng : IO ()
m2ng = ?rhs_m2ng
