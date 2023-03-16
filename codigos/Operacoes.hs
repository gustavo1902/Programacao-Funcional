module Teste where
import Data.Semigroup (diff)

soma :: Int -> Int -> Int
soma x y = x + y

subtrai :: Int -> Int -> Int
subtrai x y = x - y

multiplica :: Int -> Int -> Int
multiplica x y = x * y

divide :: Int -> Int -> Int
divide x y = div x y

--função que dobra o valor de x
dobra x = x + x

quadruplica x = dobra (dobra x)

fatorial n = product [1..n]

raizquadrada x = sqrt x

raizcubica x = x ** (1/3)

areaquadrado x = x * x

arearetangulo x y = x * y

areacirculo x = pi * (x * x)

quantosporcento :: Int -> Int -> String
quantosporcento x y = show (div (x * 100) y) ++ "%"

impar :: Integral a => a -> Bool
impar n = n `mod` 2 == 1

--só funciona com equações que possuem soluções reais
{-raiz2Grau :: Floating a => a -> a -> a -> (a, a)
raiz2Grau a b c = (((-b) + sqrt(b^2-4*a*c))/(2*a),
                   ((-b) - sqrt(b^2-4*a*c))/(2*a))
-}
raiz2Grau :: Floating a => a -> a -> a -> (a,a)
raiz2Grau a b c = (x1, x2)
    where delta = b^2-4*a*c
          x1 = (-b + sqrt delta) / (2*a)
          x2 = (-b - sqrt delta) / (2*a)

{- utilizando condicionais
raiz2Grau :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau a b c
    | delta < 0 = (0, 0)
    | otherwise = (x1, x2)
    where delta = b^2 - 4*a*c
          x1 = (-b + sqrt delta) / (2*a)
          x2 = (-b - sqrt delta) / (2*a)

-}
--distância euclidiana
euclidiana :: Floating a => a -> a -> a
euclidiana x y = sqrt diffSq
    where
        diffSq=(x-y)^2


