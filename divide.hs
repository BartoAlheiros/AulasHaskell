divide::Int->Int->Bool
divide a b 
  | mod a b ==0 = True
  | mod a b /= 0 = False

listaMenores::Int->[Int]
listaMenores a
  | a==0 = []
  | a==1 = [1]
  | otherwise = a:(listaMenores (a-1)) 

listDivi::Int->[Int]->[Int]
listDivi a [] = []
listDivi a (x:xs)
  | mod a x == 0 = x:listDivi a xs
  | otherwise = listDivi a xs

listDiviComm::Int->Int->[Int]
listDiviComm a b 
  | (mod a x == 0 && mod b x == 0) =  x:listDiviComm a b
  | otherwise = listDiviComm a b