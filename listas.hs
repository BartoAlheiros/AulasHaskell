type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo::BancoDados
baseExemplo = [("Sergio", "O Senhor dos Aneis"), ("Andre", "Duna"), ("Fernando", "Jonathan Strange & Mr. Norrell"), ("Fernando", "Duna")]

livros::BancoDados -> Pessoa -> [Livro]

livros[] p = []
livros(x:xs) p
	| fst x == p = (snd x):livros xs p
	| otherwise = livros xs p

livrosComp bd p = [l | (x,l) <- bd, x==p]

emprestimos::BancoDados -> Livro -> [Pessoa]

emprestimos[] l = []
emprestimos(x:xs) l
	| snd x == l = fst x:(emprestimos xs x)
	| otherwise = emprestimos xs x

empComp bd l = [l | (x,l) <- bd, x==p]	

listaQuadInf::[Int] -> [Int]

listaQuadInf xs = [a*a|a <- xs]

--contaOcorr::[[t],[t],[t]] -> [(t,Int)]

--contaOcorr concat

