module Main where

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

quadrado x = x * x

cubo x = x * x * x

raizquadrada x = sqrt x

raizcubica x = x ** (1/3)

areaquadrado x = x * x

arearetangulo x y = x * y

quantosporcento :: Int -> Int -> String
quantosporcento x y = show (div (x * 100) y) ++ "%"
