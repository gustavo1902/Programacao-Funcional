--1)
f1 :: Double -> Double
f1 x
  | x >=0 = (x+4)/(x+2)
  | otherwise = 2/x

f2 :: Double -> Double -> Double
f2 x y
  | x >= y = x + y
  | otherwise = x - y

f3 :: Double -> Double -> Double -> Double
f3 x y z
  | (x+y)>z = x+y+z
  | (x+y)<z = x-y-z
  | (x+y)==z = 0

--2)
--Claúsula de para na função fatorial
fat :: Int->Int
fat 0 = 1
fat x = x * fat(x-1)

--3)
soma::Int->Int->Int
soma x y = x + y

multiplica :: Int -> Int -> Int
multiplica x y
          | y ==0 = 0
          | otherwise = soma x (multiplica x(y-1))

square :: Int -> Int 
square x = x * x

--4)
invert :: Int -> Int
invert = read . reverse . show

--5)
fourPower :: Int -> Int
fourPower x = square (square x)

--6)
sqtrSeq :: Int -> Double
sqtrSeq 0 = sqrt 6
sqtrSeq i = sqtrSeq (i-1) + sqrt 6

--7) 
escolha :: Int -> Int -> Int
escolha m n
  | n == 0 || n == m = 1
  | n > m = 0
  | otherwise = escolha (m-1) (n-1) + escolha (m - 1) n
