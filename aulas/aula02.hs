vendas :: Int -> Int -- recebe um número para ser o parâmetro de x e retorna o respectivo valor.
vendas x             -- recebe o valor informado para x
    | x == 0 = 12    -- executa compação, caso seja igual será retorna o valor
    | x == 1 = 15
    | x == 2 = 20
    | x == 3 = 25
    | otherwise  = 0 -- caso o valor passado de x não seja um dos anteriores, retorna 0 como um método de parada para a função

{- 
a função vendas retorna a 
quantidade para um determinado valor de x
-}

totalVendas :: Int -> Int
totalVendas n
    | n == 0 = vendas 0                   -- método de parada
    | n > 0 = totalVendas (n-1) + (vendas n) -- 
    | otherwise = 0

{-
Soma dos inteiros de 0 a N:
sumi 0 = 0
sumi n = 0 + 1 + 2 + ... + (n-1) + n
sumi n = sumi (n-1) + n
-}
sumi :: Int -> Int 
sumi n 
    | n <= 0 = 0
    | otherwise = n + sumi (n-1)

--Fatorial de n
fatorial :: Integer -> Integer
fatorial n 
    | n <= 0 = 1
    | otherwise = n * fatorial (n-1)
