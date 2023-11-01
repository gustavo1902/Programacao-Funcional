--1
ehPrimo :: Int -> Bool
ehPrimo x 
    | mod x x ==0 && mod x 1 == 0 = True
    | otherwise = False

--2
ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordenaEmTupla a b c d = (min4 a b c d, min3 a b c d, min2 a b c d, max4 a b c d)

min2 :: Int -> Int -> Int -> Int -> Int
min2 a b c d = min a (min b (min c d))

min3 :: Int -> Int -> Int -> Int -> Int
min3 a b c d = min a (min b (min c (min d a)))

min4 :: Int -> Int -> Int -> Int -> Int
min4 a b c d = min a (min b (min c d))

max4 :: Int -> Int -> Int -> Int -> Int
max4 a b c d = max a (max b (max c d))

--3
quantosDias :: Int -> Int
quantosDias x
    | mod x 4 == 0 = 366
    | otherwise = 365 

--4
diasMes :: Int -> Int -> Int
diasMes ano mes
    | elem mes [4, 6, 9, 11] = 30
    | mes == 2 = if anoBissexto then 29 else 28
    | otherwise = 31
    where anoBissexto = (mod ano 4 == 0)

--5
dia :: Int -> Int -> Int -> Int
dia ano mes diaDoMes


