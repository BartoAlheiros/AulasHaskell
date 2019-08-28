uniListas::[[t]] -> [t]

uniListas [] = []
uniListas (x:xs) = x ++ (uniListas xs)

ocorr::Eq t=> [t] -> t -> Int

ocorr [] n = 0
ocorr (x:xs) n | n == x = 1 + (ocorr xs n)
               | otherwise = ocorr xs n

geraTupla::Eq t => [t] -> [(t,Int)]

geraTupla [] = []
geraTupla (x:xs) = (x,(ocorr (x:xs) x)):(geraTupla (filter (/=x) xs))

geraTuplaFinal xs = geraTupla (uniListas xs)