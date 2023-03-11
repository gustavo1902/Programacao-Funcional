data Pessoa = Pessoa {
  nome :: String,
  sexo :: Char,
  pai :: Maybe Pessoa,
  mae :: Maybe Pessoa
}

geracoes :: Pessoa -> [Pessoa]
geracoes p = p : (filhos >>= geracoes)
  where filhos = [f | f <- [pai p, mae p], f /= Nothing]

-- Exemplo de uso:
p1 = Pessoa "João" 'M' Nothing Nothing
p2 = Pessoa "Maria" 'F' Nothing Nothing
p3 = Pessoa "José" 'M' (Just p1) (Just p2)
p4 = Pessoa "Ana" 'F' Nothing Nothing
p5 = Pessoa "Pedro" 'M' (Just p3) (Just p4)

-- gerar árvore genealógica a partir do nó raiz
arvoreGenealogica :: Pessoa -> String
arvoreGenealogica p = concat $ map (formatar "") (geracoes p)
  where
    formatar prefixo p = prefixo ++ nome p ++ "\n" ++ filhos prefixo (pai p) (mae p)
    filhos prefixo (Just p1) (Just p2) = concat $ map (formatar (prefixo ++ "    |")) (geracoes p1 ++ geracoes p2)
    filhos prefixo (Just p1) _ = concat $ map (formatar (prefixo ++ "    |")) (geracoes p1)
    filhos prefixo _ (Just p2) = concat $ map (formatar (prefixo ++ "    |")) (geracoes p2)
    filhos _ _ _ = ""

-- Exemplo de uso:
main :: IO ()
main = putStrLn $ arvoreGenealogica p5
