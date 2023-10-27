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

fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * fatorial(x-1)

ehPalindromo :: String -> Bool
ehPalindromo x = reverse x == x

ehPalindromo :: Integer -> Bool
ehPalindromo x = 