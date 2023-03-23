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
