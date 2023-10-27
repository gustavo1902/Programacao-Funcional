listaReverse :: [Int]->[Int]
listaReverse x = reverse x

listaReversa :: [Int]->[Int]
listaReversa [] = []
listaReversa (x:xs) = listaReversa xs ++ [x]

somaImpares :: [Int] -> Int
somaImpares [] = 0
somaImpares (x:xs)
    | odd x = x + somaImpares xs
    | otherwise = somaImpares xs


somaPares :: [Int] -> Int
somaPares []= 0
somaPares (x:xs)
    | even x = x + somaPares xs
    | otherwise = somaPares xs
