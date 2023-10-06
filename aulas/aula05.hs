--listas

retornaNElementos :: [a] -> Int -> [a]
retornaNElementos _ 0 = []  -- Caso base: quando n é zero, retorna uma lista vazia
retornaNElementos [] _ = [] -- Caso base: quando a lista está vazia, retorna uma lista vazia
retornaNElementos (x:xs) n = x : retornaNElementos xs (n-1)
