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

shift :: ((Int,Int),Int) -> (Int,(Int,Int))
shift ((x,y),z) = (x,(y,z))

double :: [Int] -> [Int]
double [] = []
double l = (2* (head l)):(double (tail l) ) 

member :: [Int] -> Int -> Bool
member [] n = False
member l n 
  | (head l) == n = True
  | otherwise = member (tail l) n

digits :: String -> String
digits [] = []
digits l
  | (head l) >= '0' && (head l) <= '9' = (head l):(digits (tail l))
  | otherwise = (digits (tail l))

--segunda opção usar fst x e snd x
sumPairs [] = []
sumPairs ((x,y):xs) = (x+y):sumPairs xs

myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

myTakeWhile f [] = []
myTakeWhile f (x:xs)
  | f x == False = myTakeWhile f []
  | otherwise = x:myTakeWhile f xs

-- Continuar essa
myDropWhile f [] = []
myDropWhile f (x:xs)
  | f x == True = myDropWhile f xs
  | otherwise = x:myDropWhile f xs        

numFactors n = 1 + (numFactorsAux n (n-1))
numFactorsAux n 1 = 1
numFactorsAux n x
  | (mod n x) == 0 = 1 + numFactorsAux n (x-1)
  | otherwise = numFactorsAux n (x-1)

numFactorsComp n = length ([x | x <- [1..n], (mod n x)==0])   

-- maior n x = (n>x) 

retornaSup n [] = 0 
retornaSup n (x:xs)
  | (x > n) = 1 + retornaSup n xs
  | otherwise = retornaSup n xs

retornaSupComp n l = length([x | x <- l, x>n])

uniao [] [] = []
uniao xs [] = xs
uniao [] ys = ys
uniao (x:xs) ys
  | elem x ys = uniao xs ys
  | otherwise = x:uniao xs ys

maiores n [] = []
maiores n (x:xs)
  | (x>n) = x:maiores n xs
  | otherwise = maiores n xs

somaLista [] = 0
somaLista (x:xs) = x + somaLista xs   

mediaMaiores n xs = (somaLista (maiores n xs)) / retornaSup n xs

myTake n [] = []
myTake 0 xs = []
myTake 0 [] = []
myTake n (x:xs) = x:myTake (n-1) xs 

intercala [] [] = []
intercala xs [] = xs
intercala [] ys = ys
intercala (x:xs) (y:ys)
  | length (x:xs) == length (y:ys) = x:intercala xs (y:ys)
  | length (x:xs) > length (y:ys) = x:intercala xs ys
  | otherwise = y:intercala (x:xs) ys



-- contaOcorr [t] -> [(t,Int)]
-- contaOcorr [] = []

-- recebe uma lista de listas e devolve seu n-ésimo elemento      
-- takeElemN [] n = []
-- takeElemN xs n

pElem xs = head (pElemAux xs)

-- funcao que retorna o primeiro elemento da lista concatenada
pElemAux xs = uniaoN xs

-- concatena a lista
uniaoN [] = []
uniaoN (x:xs) = x ++ uniaoN xs

-- contaOcorrAux [] = 0
-- contaOcorrAux xs = contaOcorrAux2 xs (uniaoN (xs))   

-- contaOcorrAux2 [] [] = 0
-- contaOcorrAux2 [] ys = 0
-- contaOcorrAux2 xs [] = 0
-- contaOcorrAux2 (x:xs) (y:ys)
--  | [x] == [y] = 1 + contaOcorrAux2 xs ys
--  | otherwise = contaOcorrAux2 xs ys

-- conta3 [] [] = 0
-- conta3 xs [] = 0
-- conta3 [] ys = 0
-- conta3 

-- valores [] [] = []
-- valores (x:xs) (y:ys)
--  | fst y /= x = ((fst y) , 0 + contaOcorr xs):valores xs ys (length ys)
--  | y == x = ((y) , 1 + contaOcorr ys):valores xs ys (length ys)

-- devolve a primeira lista da lista de listas
-- passada como parâmetro ou, simplismente, o primeiro elemento de uma lista qualquer.  
quebraL [] = []
quebraL (x:xs) = x

-- itera sobre a lista e conta as ocorrências
iteraOcorr [] [] = 0
iteraOcorr xs [] = 0
iteraOcorr [] ys = 0
iteraOcorr (x:xs) ys
  | [x] == ys = 1 + (iteraOcorr xs ys)
  | otherwise = iteraOcorr xs ys

recebeL [] = 0
recebeL l = listaElem (quebraL l) (uniaoN l)

-- devolve uma lista de elementos da lista
-- passada como parâmetro, sem repetição de elementos
listaElem [] [] = 0
listaElem xs [] = 0
listaElem [] ys = 0
listaElem (x:xs) (y:ys) 
  | x == y = 1 + listaElem xs ys
  | otherwise = listaElem xs ys    

mySnd [] = []
mySnd (x:(k:xs)) = [k]

-- myOrd c = ord c

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O_Senhor_dos_Aneis"),("Andre","Duna"), ("Fernando","Jonathan_Strange_&_Mr._Norrell"),("Fernando","Duna")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] "" = []
livros [] k = []
livros xs "" = []
livros ((p,l):xs) k
  | p == k = l:livros xs k 
  | otherwise = livros xs k

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] "" = []
emprestimos [] k = []
emprestimos xs "" = []
emprestimos ((p,l):xs) k
  | l == k = p:emprestimos xs k
  | otherwise = emprestimos xs k

emprestado :: BancoDados -> Livro -> Bool
emprestado [] "" = False
emprestado db "" = False
emprestado [] c = False
emprestado ((p,l):db) c
  | l == c && p /= "" = True
  | l == c && p == "" = False
  | otherwise = emprestado db c

-- extra
numExemplares :: BancoDados -> Livro -> Int
numExemplares [] "" = 0
numExemplares db "" = 0
numExemplares [] k = 0
numExemplares ((_,l):db) k
  | l == k = 1 + numExemplares db k
  | otherwise = numExemplares db k

qtdEmprestLivro :: BancoDados -> Livro -> Int  
qtdEmprestLivro [] "" = 0
qtdEmprestLivro [] k = 0
qtdEmprestLivro db "" = 0
qtdEmprestLivro ((_,l):db) k
  | l == k = 1 + qtdEmprestLivro db k
  | otherwise = qtdEmprestLivro db k  

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] "" = 0
qtdEmprestimos [] k = 0
qtdEmprestimos db "" = 0
qtdEmprestimos ((p,l):db) k
  | p == k = 1 + qtdEmprestimos db k
  | otherwise = qtdEmprestimos db k

-- ainda nao tah finalizada. Sugestao eh criar uma funcao
-- emprestarAux, que devolva uma copia do banco
-- emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
-- emprestar [] "" "" = []
-- emprestar [] k c = []
-- emprestar db "" "" = db
-- emprestar db "" c = db
-- emprestar db k "" = db
-- emprestar ((p,l):db) k c
--  | l == c && p == "" = ((k,c)):db         

-- Questão 1
somaPares :: [Int] -> [Int] -> [Int]
somaPares [] [] = []
somaPares xs [] = xs ++ []
somaPares [] ys = [] ++ ys
somaPares (x:xs) (y:ys) = (x+y):somaPares xs ys

-- Questão 2
sublista :: [Char] -> [Char] -> Bool
sublista xs ys = sublistaAux xs ys (length xs) (length ys)

sublistaAux [] [] 0 0 = True 
sublistaAux xs [] n p = False
sublistaAux [] ys 0 p = True
sublistaAux xs ys n p
  | (xs == ys) = True 
  | otherwise = sublistaAux xs (tail ys) n p   

-- Questão 3
--crescente :: (Int -> Int) -> Int -> Bool
--crescente  (0 , _) n = crescenteAux p 0 n -- Como eu passo essa função como argumento?

--crescenteAux p x

--func1 x = x + 1

--sqrtL :: [t] -> [t]
sqrtL xs = map (\n->n*n) xs	

--dobroSoma (x:xs) = foldl (\n->n*n) x xs 



--membro :: [Int] -> Int -> Bool
membro l k = elem k l 

livrosSegOrd banco p = map snd (filter (\(x,y) -> x==p) banco)

-- funcoes de segunda ordem
total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = total f (n-1) + f n

sumSquares :: Int -> Int
sumSquares n = total sq n

-- essa funcao eleva um dado valor ao quadrado
-- mas ela nao estah considerando valores menores que 0
sq :: Int -> Int
sq a = a*a

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f n = f n > f (n-1)

--1)    Faça uma função que dada uma string e um caractere 
--retorna uma nova string onde todas as ocorrências de do caractere são removidas.
equal_remove xs c = filter (\x -> x /= c) xs

--2)    Faça uma função que retorna se todos os inteiros de uma lista são pares
all_even xs = foldl1 (&&) (map (\x -> even x) xs)

--3)    Faça uma função que dada uma lista de números, 
-- retorna uma lista que contém a raiz quadrada dos elementos que são maiores do que zero.
maiores_zero_sqrt :: [Float] -> [Float]
maiores_zero_sqrt xs = map (sqrt) (filter (\x -> x > 0) xs)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add a b) = (showExpr (a)) ++ ['+'] ++ (showExpr (b))
showExpr (Sub j k) = (showExpr(j)) ++ ['-'] ++ (showExpr(k))

data List t = Nil | Cons t (List t)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons t l) = t:toList (l)

safeDiv :: Integral t => Maybe t -> Maybe t -> Maybe t
safeDiv _ Nothing = Nothing
safeDiv Nothing _ = Nothing
safeDiv (Just x) (Just y)
  | y /= 0 = Just (x `div` y)
  | otherwise = Nothing

data Fila t = Fila (Int,[t])

push x (Fila (TAM,y))
  | (length y) == TAM = Left "Fila Cheia!"
  | otherwise = Right(x,Fila((TAM), y ++ [x]))