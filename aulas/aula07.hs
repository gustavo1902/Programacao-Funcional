--1
isEven :: Int -> Bool
isEven a
    | mod a 2 == 0 = True
    | otherwise = False

--2
maxi2 :: Int -> Int -> Int
maxi2 x y
    | x >= y = x
    |otherwise = y

--3
nonEmpty :: String -> Bool
nonEmpty x
    | length x <= 0 = False
    | otherwise = True