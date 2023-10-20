listaReverse :: [Int]->[Int]
listaReverse x = reverse x

listaReversa :: [Int]->[Int]
listaReversa [] = []
listaReversa (x:xs) = listaReversa xs ++ [x]

somaImpares :: [Int] -> Int
somaImpares [] = 0
somaImpares (x:xs)
    |