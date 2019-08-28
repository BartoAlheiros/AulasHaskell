-- 1a questão exercício 01 - 1a VA 
retornaSomaInf::Int -> [Int] -> Int
retornaSomaInf n [] = 0
retornaSomaInf n (x:xs) = somaLista (geraLista (x:xs) n)

-- soma os elementos de uma lista passada como parametro
somaLista::[Int] -> Int
somaLista [] = 0
somaLista (x:xs) = head (x:xs) + somaLista xs

-- cria uma nova lista com os valores inferiores a n
-- de uma lista passada como parametro
geraLista::[Int] -> Int -> [Int]
geraLista [] n = []
geraLista (x:xs) n 
               | x < n = x:geraLista xs n                   	
               | otherwise = geraLista xs n

-- 2a questão exercício 01 - 1a VA 
-- conta ocorrencias de um número em uma lista
contaOcorrencias::Int->[Int]->Int
contaOcorrencias _ [] = 0
contaOcorrencias x (y:ys)
		| x == y = 1 + contaOcorrencias x ys
		| otherwise = contaOcorrencias x ys

removeDaLista::Int->[Int]->[Int]
removeDaLista _ [] = []
removeDaLista x (y:ys)
		| x /= y = y:removeDaLista x ys
		| otherwise = removeDaLista x ys

ocorreMaisDeUma::[Int]->[Int]
ocorreMaisDeUma [] = []
ocorreMaisDeUma (x:xs)
		| ((contaOcorrencias x xs) + 1) > 1 = x:ocorreMaisDeUma (removeDaLista x xs)
		| otherwise = ocorreMaisDeUma xs  

-- 3a questão exercício exercício 01 - 1a VA 
divideESomaNumero::Int->Int
divideESomaNumero 0 = 0
divideESomaNumero x 
        | x > 999 = (div x 1000) + divideESomaNumero(x - (div x 1000) * 1000)
        | x > 99 = (div x 100) + divideESomaNumero(x - (div x 100) * 100) 
	 	| x > 9 = div x 10 + divideESomaNumero(x - (div x 10) * 10)
	    | otherwise = x 

saoMultiplos::(Int,Int)->Bool
saoMultiplos (_,0) = False
saoMultiplos (x,y)
		| mod x y == 0 = True
		| otherwise = False   

digitosDeMultiplos::[Int]->[(Int,Int)]
digitosDeMultiplos[] = []
digitosDeMultiplos (x:xs) 
		| saoMultiplos (x,divideESomaNumero x) == True = (x,divideESomaNumero x):digitosDeMultiplos xs
		| otherwise = digitosDeMultiplos xs 
    