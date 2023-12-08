--1
ehPerfeito :: Int -> Bool
ehPerfeito n = somaDivisores == n
 where
   divisores :: Int -> Int -> Int -> Int
   divisores x acc i
     | i == x = acc
     | x `mod` i == 0 = divisores x (acc + i) (i + 1)
     | otherwise = divisores x acc (i + 1)
  
   somaDivisores = divisores n 0 1

--2
contaString :: String -> [(Char, Int)]
contaString str = contaOcorrencia str []
   where
       contaOcorrencia [] acc = acc
       contaOcorrencia (x:xs) acc = contaOcorrencia xs (atualizaOcorrencia x acc)
      
       atualizaOcorrencia :: Char -> [(Char, Int)] -> [(Char, Int)]
       atualizaOcorrencia c [] = [(c, 1)]
       atualizaOcorrencia c ((ch, cnt):xs)
           | c == ch   = (ch, incrementaContagem cnt) : xs
           | otherwise = (ch, cnt) : atualizaOcorrencia c xs
      
       incrementaContagem :: Int -> Int
       incrementaContagem n = n + 1


--3
inverte :: String -> String
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]


--4
-- map square [1,2,3,4,5]
square :: Int -> Int
square x = x ^ 2

--5
produtoCartesiano :: [Int] -> [Int] -> [(Int, Int)]
produtoCartesiano xs ys = [(x, y) | x <- xs, y <- ys] ++ [(y, x) | x <- xs, y <- ys]


--6
--filter positives [-1,2,-3,4,-5]
positives :: Int -> Bool
positives x = x > 0


--7
--foldr1 (+) (map sumDouble [1,-2,3])
sumDouble :: Int -> Int 
sumDouble x = x * 2

--8
--foldr1 concatena ["tes","ta"]
concatena :: String -> String -> String
concatena a b = a ++ b