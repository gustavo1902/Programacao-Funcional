{-# LANGUAGE FlexibleContexts #-}

{-
Programming in Haskell by Graham Hutton Exercises 
versão traduzida

1. Usando uma compreensão de lista, dê uma expressão que calcula a soma 1^2 + 2^2 + ... 100^2 dos primeiros cem quadrados inteiros.
-}
{- Ppssível resolução
main :: IO ()
main = do
    let result = sum [x^2 | x <- [1..100]]
    print result
-}


{-
2. De maneira similar à função length, mostre como a função da biblioteca replicate :: Int -> a -> [a] que produz uma lista de elementos idênticos pode ser definida usando uma compreensão de lista. Por exemplo:
> replicate 3 True
[True, True, True]
-}
replicate' n x = [x | _ <- [1..n]]

{-
3. Uma tripla (x, y, z) de inteiros positivos é pitagórica se x^2 + y^2 = z^2. Usando uma compreensão de lista, defina uma função pyths :: Int -> [(Int, Int, Int)] que retorna a lista de todas as triplas pitagóricas cujos componentes são no máximo um limite dado. Por exemplo:

> pyths 10
[(3,4,5), (4,3,5),(6,8,10),(8,6,10)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

{-Explicação: A compreensão de lista gera todas as combinações possíveis de (x, y, z) dentro dos limites 1 a n e verifica se eles satisfazem a condição x^2 + y^2 == z^2, que é a definição de uma tripla pitagórica. O resultado é uma lista contendo todas as triplas pitagóricas até o limite dado.-}

{-
4. Um número inteiro positivo é perfeito se ele é igual à soma de seus fatores, excluindo o próprio número. Usando uma compreensão de lista e a função factors, defina uma função perfects :: Int -> [Int] que retorna a lista de todos os números perfeitos até um limite dado. Por exemplo:
> perfects 500
[6,28,496]
-}
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

{-Primeiramente definimos a função factors  que recebe um inteiro n e retorna uma lista com todos os seus fatores. 
Depois definimos a função perfects que cria uma lista de números de 1 até n, e para cada número x, verifica se a soma de seus fatores é igual a ele próprio. Se for, o número é adicionado à lista de números perfeitos.-}

{-
5. Mostre como a única compreensão [(x,y) | x <- [1,2,3], y <- [4,5,6]] com dois geradores pode ser reescrita usando duas compreensões com geradores únicos. Dica: use a função de biblioteca concat e aninhe uma compreensão dentro da outra.
Eu devo admitir que fiquei totalmente confuso com esta.
[[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]
>[[(1,4),(1,5),(1,6)],[(2,4),(2,5),(2,6)],[(3,4),(3,5),(3,6)]]
-}
gerarPares :: [(Int, Int)]
gerarPares = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
{-A expressão original [(x,y) | x <- [1,2,3], y <- [4,5,6]] tem dois geradores, um para x e outro para y, e produz uma lista de tuplas.

Uma maneira de reescrevê-la com geradores únicos seria aninhar uma compreensão dentro da outra, de forma que a primeira gera apenas os valores de x, e a segunda gera os valores de y para cada x. Assim, temos:-}



{-
6. Defina a função find usada na função positions.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
where n = (length xs) - 1
-}
find :: Eq a => a -> [(a,b)] -> [b]
find x pairs = [b | (a,b) <- pairs, a == x]

{-Essa função recebe um valor x e uma lista de tuplas (a,b). Ela retorna uma lista contendo o segundo elemento de cada tupla cujo primeiro elemento é igual a x. Em outras palavras, ela retorna uma lista com todas as posições em que o valor x aparece na lista original.

Na função positions, ela é usada para encontrar as posições em que um determinado elemento x aparece em uma lista xs juntamente com sua posição correspondente usando a função zip. A variável n é usada para determinar a última posição da lista xs, que é usada para definir os índices das posições.-}

{-
7. O produto escalar de duas listas de inteiros xs e ys de comprimento n é dado pela soma dos produtos dos inteiros correspondentes.
n = 1
--
\ (xsi * ysi)
/
--
i = 0
De maneira similar à função chisqr, mostre como uma lista de compreensão pode ser usada para definir uma função scalarproduct :: [Int] -> [Int] -> Int que retorna o produto escalar de duas listas. Por exemplo:
> scalarproduct [1,2,3] [4,5,6]
32
-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
{-Aqui, usamos a função zip para criar uma lista de tuplas em que cada tupla contém dois inteiros correspondentes de xs e ys. Em seguida, usamos uma compreensão de lista para multiplicar cada par de inteiros e, finalmente, usamos a função sum para somar todos os produtos.-}

{-
8. 1. Defina o operador de exponenciação &! para números inteiros não negativos usando o mesmo padrão de recursão que o operador de multiplicação *, e mostre como 2 &! 3 é avaliado usando sua definição.
-}
(&!) :: Int -> Int -> Int
m &! 0 = 1
m &! n = m * (m &! (n-1))

result :: Int
result = 2 &! 3




{-
9. 1. Mostre como a lista de compreensão [f x | x <- xs, p x] pode ser reescrita usando as funções de ordem superior map e filter. Tente entender e aplicar o exemplo [(+7) x | x <- [1..10], odd x]
-}



{-10. 4. Defina uma função dec2int :: [Int] -> Int que converte um número decimal em um inteiro. por exemplo:
> dec2int [2,3,4,5]
2345
-}
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

{-
11. Uma função de ordem superior unfold que encapsula um padrão simples de recursão para produzir uma lista pode ser definida da seguinte forma:
unfold p h t x | p x = []
|otherwise = h x : unfold p h t (t x)
Defina novas funções para que você possa chamar a função unfold passando uma lista x como parâmetro e novas funções para passar um elemento de tipo primitivo x.
-}
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

unfoldList :: Eq a => [a] -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> [b]
unfoldList xs p h t = unfold null (unfoldElem (head xs) p h t) t xs


unfoldElem :: Eq a => a -> (a -> Bool) -> (a -> b) -> (a -> [a]) -> [b]
unfoldElem x p h t = unfold p (h . head) (concatMap t . take 1 . dropWhile (not . null) . iterate t) [x]
{-Código apresenta erros-}
