a :: Int -> Int
a a = a 

soma :: Int -> Int -> Int
soma a b = a + b

subtracao :: Int -> Int -> Int
subtracao a b = a - b

multiplica :: Int -> Int -> Int
multiplica a b = a * b

divide :: Double -> Double -> Maybe Double
divide a b
    | b == 0    = Nothing
    | otherwise = Just (a / b)


square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a==b) && (a==c)

maxi :: Int -> Int -> Int
maxi a b 
    | a < b = b
    |otherwise = a

min :: Int -> Int -> Int
min a b
    | a > b = b
    | otherwise = a

raizesReais :: Double -> Double -> Double -> Int
raizesReais a b c
    | delta > 0  = 2  -- Duas raízes reais distintas
    | delta == 0 = 1  -- Uma única raiz real (raiz dupla)
    | otherwise  = 0  -- Nenhuma raiz real
  where
    delta = b^2 - 4*a*c

distanciaEuclidiana :: (Double, Double) -> (Double, Double) -> Double
distanciaEuclidiana (x1, y1) (x2, y2)
    | x1 == x2  = abs (y2 - y1)  -- Caso x1 seja igual a x2
    | y1 == y2  = abs (x2 - x1)  -- Caso y1 seja igual a y2
    | otherwise = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..(floor $ sqrt $ fromIntegral n)]

primeNumbers :: Integer -> [Integer]
primeNumbers n = take (fromIntegral n) [x | x <- [2..], isPrime x]

isEven :: Int -> Bool
isEven a
    | mod a 2 == 0 = True
    | otherwise = False

isOdd :: Int -> Bool
isOdd a
    | a `mod` 2 == 0 = False
    | otherwise = True




