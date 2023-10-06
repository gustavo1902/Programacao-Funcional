--listas

-- 1. Faça uma função que receba uma lista e um número n e retorne os n primeiros elementos da lista.
retornaNElementos :: [a] -> Int -> [a]
retornaNElementos _ 0 = []  -- Caso base: quando n é zero, retorna uma lista vazia
retornaNElementos [] _ = [] -- Caso base: quando a lista está vazia, retorna uma lista vazia
retornaNElementos (x:xs) n = x : retornaNElementos xs (n-1)

-- 2. Faça uma função para inverter os elementos de uma lista.
inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

-- 3. Construa uma função zip que agrupe duas listas, elemento a elemento, combinando-os em pares (tupla-2), gerando uma única lista ao final.
agrupaListas :: [a] -> [b] -> [(a,b)]
agrupaListas [] _ = []
agrupaListas _ [] = []
agrupaListas (a:b) (c:d) = (a,c) : agrupaListas b d 