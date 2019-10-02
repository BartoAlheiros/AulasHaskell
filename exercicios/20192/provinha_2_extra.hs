-- Tipos Algébricos e Monads 1
data SisArq s = Dir (String, [String]) | Arq String

--buscaArquivo :: SisArq -> SisArq -> String
buscaArquivo (Arq arq) Dir (nome_dir, list_arq)
  | buscaEmLista (arq, list_arq) /= "" = "/" ++ nome_dir
  | otherwise = putStrLn "Arquivo não encontrado"


-- busca o arquivo na lista de arquivos do diretorio passado
-- como parametro
-- recebe um Arquivo e a lista de arquivos do diretorio
--buscaEmLista :: SisArq -> String -> SisArq
buscaEmLista (Arq arq) [] = ""
buscaEmLista (Arq arq) (x:list_arq)
  | arq == x = x
  | otherwise = buscaEmLista arq list_arq 