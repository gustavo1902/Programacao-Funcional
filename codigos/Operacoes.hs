module Teste where

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
