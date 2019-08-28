negativo::Int -> Int

negativo n = 0-n

maxi::Int -> Int -> Int

maxi n m | n >= m    = n
         | otherwise = m

-- funcão intermediária, que recebe uma lista
-- gerada por geraLista e um inteiro n, para calcular
-- seu número de divisores  
numFactorsIntermed::[Int] -> Int -> Int

numFactorsIntermed xs 1 = 1
numFactorsIntermed (x:xs) n | ehDivisor n head (x:xs) == True = 1 + numFactorsIntermed xs n  	
                        | otherwise = numFactorsIntermed xs n

-- verifica se x é divisor de y. 
ehDivisor::([a]->a) -> Int -> Bool

ehDivisor ([a]->a) x = (mod x y) == 0

geraLista::Int -> [Int]

geraLista n = [1..n]

uniListas::[[t]] -> [t]

uniListas [] = []
uniListas (x:xs) = x ++ (uniListas xs)

ocorr::String -> Char -> Int

ocorr [] n = 0
ocorr (x:xs) n | n == x = 1 + (ocorr xs n)
               | otherwise = ocorr xs n

geraTupla::String -> Char -> (Char,Int)

geraTupla [] c = (c,0)
geraTupla (x:xs) c = (c,ocorr (x:xs) c)
