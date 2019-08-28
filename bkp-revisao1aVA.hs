ocorreMaisDeUma::[Int]->[Int]
ocorreMaisDeUma [] = []
ocorreMaisDeUma (x:xs)
		| (contaOcorrencias x xs + 1) > 1 = x:	