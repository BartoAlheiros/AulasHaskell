fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat(n-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (b == c) && (c == d)

equalCount :: Int -> Int -> Int -> Int
equalCount a b c
  | ((a == b) && (b == c)) = 3
  | (a == b) || (b == c) || (a == c) = 2
  | otherwise = 0

numVendas :: Int -> Int -> Int
vendas 1 = 30
vendas 2 = 30
vendas 3 = 60
vendas 4 = 10

numVendas s 0 = 0
numVendas s n
  | ( s == vendas n ) = 1 + ( numVendas s (n-1)) 
  | otherwise = numVendas s (n - 1)

-- multiplica o número por 10000, aplica round e depois divide por 10000
-- ver exemplo com 13.666667 -> 
-- 13.666667 * 10000 = round 136666.67
-- 136667
-- 136667/10000 = 13.6667
round4dp :: Float -> Float
round4dp x = fromIntegral (round $ x * 1e4) / 1e4

media3 :: Int -> Int -> Int -> Int
media3 a b c = ( (div (a + b + c)) (fromIntegral (3)) )

media32 :: Float -> Float -> Float -> Float
media32 a b c = round4dp((a + b + c) / 3)
-- countAboveMed :: Int -> Int ->   

addEspacos :: Int -> String
addEspacos n
  | n == 0 = ""
  | otherwise = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n s = (addEspacos n) ++ s



cabecalho :: Int -> String
cabecalho a = "Semana \t" ++ "Vendas \n"

-- imprimeSemanas :: Int -> 

somaVendas :: Int -> Int
semana 0 = 12
semana 1 = 14
semana 2 = 15

somaVendas n
  | (n == 0) = semana 0
  |	otherwise = (semana n) + somaVendas (n-1)

imprimeTotal :: Int -> String
imprimeTotal a = "Total \t" ++ show(somaVendas a) ++ "\n"

mediaVendas :: Float -> Float -> Float -> Float
mediaVendas a b c = (media32 a b c)     

imprimeMedia :: Int -> String
imprimeMedia a = "Média \t" ++ show(mediaVendas (semana 0) (semana 1) (semana 2)) ++ "\n"

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho n
                ++ imprimeSemanas n
                ++ imprimeTotal n
                ++ imprimeMedia n

imprimeFinal n = putStr(imprimeTabela n)

-- imprimeMedia :: Int -> String
-- imprimeMedia a = "Média \t" ++ mediaVendas

imprimeSemanas :: Int -> String
imprimeSemanas n
  | (n == 0)  = show(n+2) ++ " \t" ++ show(semana (n+2)) ++ "\n"
  | (n == 1)  = show(n) ++ " \t" ++ show(semana n) ++ "\n" ++ imprimeSemanas (n-1)
  | (n == 2)  = show(n-n) ++ " \t" ++ show(semana (n-n)) ++ "\n" ++ imprimeSemanas(n-1)
--  | (n == 0) = show(0) ++ " \t" ++ show(semana 0) ++ "\n" | otherwise = show(n) ++ " \t" ++ show(semana n) ++ "\n" ++ imprimeSemanas (n-1)  
 

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
  | c > a && c > b && b > a = (a, c)
  | b > a && b > c && a > c = (b, a)
--  | a > b && a > c &&  = (b, c)

