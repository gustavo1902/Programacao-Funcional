--listas

-- 1. 
retornaNElementos _ 0 = []  
retornaNElementos [] _ = [] 
retornaNElementos (x:xs) n = x : retornaNElementos xs (n-1)

-- 2. 
inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

-- 3. 
agrupaListas :: [a] -> [b] -> [(a,b)]
agrupaListas [] _ = []
agrupaListas _ [] = []
agrupaListas (a:b) (c:d) = (a,c) : agrupaListas b d 

-- 4. 
maiorValor :: [Int] -> Int
maiorValor [] = 0
maiorValor [x] = x
maiorValor (x:xs)
    | x > resto = x
    | otherwise = resto
    where resto = maiorValor xs

-- 5.
adicionaSemRepeticao :: Eq a => [a] -> a -> [a]
adicionaSemRepeticao [] x = [x]  
adicionaSemRepeticao (x:xs) y
    | x == y = x:xs            
    | otherwise = x : adicionaSemRepeticao xs y 



    