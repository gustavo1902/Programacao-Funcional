import Data.Char 

--1
listaReversa :: [a] -> [a]
listaReversa [] = []
listaReversa (x:xs) = listaReversa xs ++ [x]

--2
duplastuplas :: [a] -> [b] -> [(a, b)]
duplastuplas [] _ = []
duplastuplas _ [] = []
duplastuplas (x1:xs1) (x2:xs2) = (x1, x2) : duplastuplas xs1 xs2

--3
digits :: String -> String
digits text = filter isDigit text

--4
letters :: String -> String
letters text = filter isLetter text


