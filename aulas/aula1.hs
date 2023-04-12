answer :: Int
answer = 42

square :: Int -> Int
square a = a * a

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a==b) && (b==c)

maxi :: Int -> Int -> Int
maxi a b 
    | a >= b = a
    | otherwise  = b
