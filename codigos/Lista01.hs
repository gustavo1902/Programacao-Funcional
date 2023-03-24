f1 :: Double -> Maybe Double
f1 x
  | x >= 0     = Just ((x + 4) / (x + 2))
  | x < 0     = Just (2 / x)
  | otherwise = Nothing
{-Precisa do otherwise? Necessário para retornar nothing quando a divisão for por 0-}

f2 :: Double -> Double -> Double
f2 x y
  | x >= y    = x + y
  | otherwise = x - y

f3 :: Double -> Double -> Double -> Double
f3 x y z
  | (x+y)>z = x+y+z
  | (x+y)<z = x-y-z
  | (x+y)==z = 0

{-
Função com problema de recursão infinita, nesse caso ela deveria parar quando o número for 0.
fat :: Int->Int
fat x = x * fat(x-1)
-}

{- Com a implementação de uma claúsula de parada-}
fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x - 1)

soma :: Int -> Int -> Int 
soma x y = x + y

mult :: Int -> Int -> Int
mult x y
  | y == 0    = 0
  | otherwise = soma x (mult x (y-1))

invertInt :: Int -> Int
invertInt n = read (reverse (show n)) :: Int

{-
invertInt :: Int -> Int
invertInt n = go n 0
  where
    go 0 acc = acc
    go n acc = go (n `div` 10) (acc * 10 + (n `mod` 10))
-}
{-Num a sifnigica que a função funciona com qualquer tipo numérico-}
square :: Num a => a -> a
square x = x * x

fourPower :: Num a => a -> a
fourPower x = square (square x)

sqrtSeq :: Int -> Double
sqrtSeq 0 = sqrt 6
sqrtSeq i = sqrtSeq (i-1) + sqrt 6

choose :: Integer -> Integer -> Integer
choose m n
  | n == 0 || n == m = 1
  | n > m = 0
  | otherwise = choose (m-1) (n-1) + choose (m-1) n

{-8: mdc com recursão
A função recebe dois argumentos m e n
A primeira linha define uma condição para parada, se n for igual a 0, então MDC é m
Caso contrário a recursão continua passando n como primeiro argumento e o resto da divisão de m por n como segundo argumento-}
mdc :: Int -> Int -> Int
mdc m n
  | n == 0 = m
  | otherwise = mdc n (m `mod`n)

{-9: Quantos múltiplos um inteiro tem em um intervalo
-}

howManyMultiples :: Int -> Int -> Int -> Int 
howManyMultiples n start end = length [x | x <- [start..end], x `mod` n == 0]

ultimoDigito :: Int -> Int
ultimoDigito x = read [last(show n)]
{-Aqui é convertido o número em string com a função show e a função last retorna o último caractere, com read converte de volta para inteiro-}

{-
ultimoDigito :: Int -> Int
ultimoDigito n = n `mod` 10

aqui o operador % retorna o resto da divisão de n por 10 que será o último digito
-}
