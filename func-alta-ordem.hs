isCrescent::(Int -> Int) -> Int -> Bool

isCrescent f 0 = True
isCrescent f n = isCrescent f (n-1) && f (n-1) < f n

--sqr::Int -> Int
--sqr n = n * n

sqrList::[Int] -> [Int]
sqrList xs = map (\x->x*x) xs

somaQuadrados::[Int] -> Int
somaQuadrados xs = foldr1 (+) (sqrList xs)

