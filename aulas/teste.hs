sumDouble :: Num a => [a] -> a
sumDouble xs = sum [x * 2 | x <- xs]

