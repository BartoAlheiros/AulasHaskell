all4Equal x y z w
    | x == y && y == z && z == w = True
    | otherwise = False

equalCount x y z
    | x == y && y == z = 3
    | x == y || y == z = 2
    | otherwise = 0

addEspacos n
    | n == 0 = ""
    | otherwise = " " ++ (addEspacos(n-1))

paraDireita :: Int -> String -> String 
paraDireita n x = (addEspacos n) ++ x      
	
--let palavra = "casa" in addEspacos

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior x y z 
    | x < y && y < z = (x, z)
    | x > y && y > z = (z, x)
    | y > x && x < z = (x, y)
    | y > x && x > z = (z, y)
    | x > y && x < z = (y, z)

ordenaTripla :: Int -> Int -> Int -> (Int,Int,Int)
ordenaTripla x y z
	| menorMaior x y z == (x, z) = (x, y, z)
	| menorMaior x y z == (z, x) = (z, y, x)
	| menorMaior x y z == (x, y) = (x, z, y)
	| menorMaior x y z == (z, y) = (z, x, y)
	| menorMaior x y z == (y, z) = (y, x, z)

dobra::[Int] -> [Int] 
dobra [] = [] 
dobra (x:xs) = (2*x) : (dobra xs)

member::[Int] -> Int -> Bool
member [] z = False
member (x:xs) z
	| x == z = True
	| otherwise = member xs z

digits::[Char] -> [Char]

digits [] = []
digits (x:xs)
	| x >= '0' && x <= '9' = x:(digits xs)
	| otherwise = digits xs

sumPairs::[(Int,Int)] -> [Int]

sumPairs [] = []
sumPairs (x:xs) = (fst x)+(snd x):(sumPairs xs)

captura::[a] -> Int -> [a]

captura [] n = []
captura t 0 = []
captura (x:xs) n = x:(pegar xs (n-1))
