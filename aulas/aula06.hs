--1. Faça uma função que retorne os n primeiros números de uma lista, sabendo que o número de elementos de uma lista vazia é zero.
nprimeiros :: Int -> [Int] -> [Int]
nprimeiros 0 _ = []
nprimeiros _ [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs

--2. Faça uma função que verifique se existe um número em uma lista.
existe :: Int -> [Int] -> Bool
existe _ [] = False
existe n (x:xs)
  | n == x    = True
  | otherwise = existe n xs

--3. Faça uma função que retorne o maior valor numérico de uma lista.
maxLista :: [Int] -> Int
maxLista [x] = x
maxLista (x:y:resto)
  | x >= y = maxLista (x:resto)
  | otherwise = maxLista (y:resto)

maiorValor :: [Int] -> Int
maiorValor [x] = x
maiorValor (x:xs) = maxLista (x:xs)

--4. Faça uma função para inverter os elementos de uma lista.
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

--5. Faça uma função que encontre o último elemento da lista.
ultimoElemento :: [Int] -> Int
ultimoElemento [x] = x
ultimoElemento (_:xs) = ultimoElemento xs

--6. Faça uma função que encontre o k-ésimo elemento de uma lista.
kelemento :: Int -> [Int] -> Int
kelemento _ [] = -1
kelemento k (x:xs)
  | k == 1    = x
  | otherwise = kelemento (k - 1) xs