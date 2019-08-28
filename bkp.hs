--sellEqualCount :: Int -> Int -> Int
--sellEqualCount s n
    --| vendas n == s = n - (n-1) 

-- let vendas 1 = 35
   -- vendas 2 = 43
    -- vendas 3 = 43
    --vendas 4 = 14
    --vendas 5 = 5 in sellEqualCount

    calculaVendasSem n
    | n == 0 = semana n
    | otherwise = semana n 

let semana 0 = 12
    semana 1 = 14
    semana 2 = 15 in calculaVendasSem

    emprestado::BancoDados -> Livro -> Bool

emprestado[] l == False
emprestado((x,l):xs) l
	| snd x == l = True
	|  