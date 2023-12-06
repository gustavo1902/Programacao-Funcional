--1
ehPerfeito :: Int -> Bool
ehPerfeito n = sum (divisores n) == n
  where divisores n = [x | x <- [1..n-1], n `mod` x == 0]

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
inverteString :: String -> String
inverteString [] = []
inverteString (x:xs) = inverteString xs ++ [x]

--4

--5
produtoCartesiano :: [Int] -> [Int] -> [(Int, Int)]
produtoCartesiano xs ys = [(x, y) | x <- xs, y <- ys] ++ [(y, x) | x <- xs, y <- ys]

--6


--7
sumDouble :: Int -> Int -> Int
sumDouble a acc = a * 2 + acc


--8
concatena :: String -> String -> String
concatena a b = a ++ b