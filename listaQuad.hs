listaQuadInf::[Int] -> [Int]

listaQuadInf xs = [a*a|a <- xs]

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo::BancoDados
baseExemplo = [("Sergio", "O Senhor dos Aneis"), ("Andre", "Duna"), ("Fernando", "Jonathan Strange & Mr. Norrell"), ("Fernando", "Duna")]

livros::BancoDados -> Pessoa -> [Livro]

livros [] p = []
livros ((x,l):xs) p
	| x == p = 