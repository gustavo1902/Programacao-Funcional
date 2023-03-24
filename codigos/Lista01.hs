import Data.Char (digitToInt) {-Exercício 11-}

f1 :: Double -> Maybe Double
f1 x
  | x >= 0     = Just ((x + 4) / (x + 2))
  | x < 0     = Just (2 / x)
  | otherwise = Nothing
{-Precisa do otherwise? Necessário para retornar nothing quando a divisão for por 0-}

f2 :: Double -> Double -> Double
f2 x y
  | x >= y    = x + y
  | otherwise = x - y

f3 :: Double -> Double -> Double -> Double
f3 x y z
  | (x+y)>z = x+y+z
  | (x+y)<z = x-y-z
  | (x+y)==z = 0

{-
Função com problema de recursão infinita, nesse caso ela deveria parar quando o número for 0.
fat :: Int->Int
fat x = x * fat(x-1)
-}

{- Com a implementação de uma claúsula de parada-}
fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x - 1)

soma :: Int -> Int -> Int 
soma x y = x + y

mult :: Int -> Int -> Int
mult x y
  | y == 0    = 0
  | otherwise = soma x (mult x (y-1))

invertInt :: Int -> Int
invertInt n = read (reverse (show n)) :: Int

{-
invertInt :: Int -> Int
invertInt n = go n 0
  where
    go 0 acc = acc
    go n acc = go (n `div` 10) (acc * 10 + (n `mod` 10))
-}
{-Num a sifnigica que a função funciona com qualquer tipo numérico-}
square :: Num a => a -> a
square x = x * x

fourPower :: Num a => a -> a
fourPower x = square (square x)

sqrtSeq :: Int -> Double
sqrtSeq 0 = sqrt 6
sqrtSeq i = sqrtSeq (i-1) + sqrt 6

choose :: Integer -> Integer -> Integer
choose m n
  | n == 0 || n == m = 1
  | n > m = 0
  | otherwise = choose (m-1) (n-1) + choose (m-1) n

{-8: mdc com recursão
A função recebe dois argumentos m e n
A primeira linha define uma condição para parada, se n for igual a 0, então MDC é m
Caso contrário a recursão continua passando n como primeiro argumento e o resto da divisão de m por n como segundo argumento-}
mdc :: Int -> Int -> Int
mdc m n
  | n == 0 = m
  | otherwise = mdc n (m `mod`n)

{-9: Quantos múltiplos um inteiro tem em um intervalo
-}

howManyMultiples :: Int -> Int -> Int -> Int 
howManyMultiples n start end = length [x | x <- [start..end], x `mod` n == 0]

ultimoDigito :: Int -> Int
ultimoDigito x = read [last(show x)]
{-Aqui é convertido o número em string com a função show e a função last retorna o último caractere, com read converte de volta para inteiro-}

{-
ultimoDigito :: Int -> Int
ultimoDigito n = n `mod` 10

aqui o operador % retorna o resto da divisão de n por 10 que será o último digito
-}

anyDigit :: Int -> Int -> Int
anyDigit pos num
  | pos < 0 = -1
  | num < 0 = anyDigit pos (abs num) -- tratando números negativos
  | otherwise = anyDigitHelper pos (show num)

anyDigitHelper :: Int -> String -> Int
anyDigitHelper pos numStr
  | length numStr <= pos = -1
  | otherwise = digitToInt (numStr !! pos)

{-
Explicação da implementação:

A função "anyDigit" recebe dois argumentos: "pos", que indica a posição do dígito a ser retornado (começando em 0), e "num", que é o número inteiro do qual se deseja extrair o dígito.

O primeiro caso da cláusula "guarda" (guard) verifica se a posição é negativa. Se for, a função retorna -1, indicando que a posição é inválida.

O segundo caso da cláusula "guarda" verifica se o número é negativo. Se for, a função chama a si mesma com o valor absoluto de "num", para tratar números negativos.

O terceiro caso da cláusula "guarda" é executado se os casos anteriores não forem atendidos. Essa cláusula chama a função auxiliar "anyDigitHelper", que recebe a posição e uma representação em string do número ("show num") e retorna o dígito correspondente, convertendo-o para inteiro com a função "digitToInt". Se a posição for inválida (maior ou igual ao tamanho da string), a função retorna -1.

A função auxiliar "anyDigitHelper" recebe a posição e a string do número como argumentos. O primeiro caso da cláusula "guarda" verifica se a posição é maior ou igual ao tamanho da string. Se for, a função retorna -1. Caso contrário, a função utiliza a função "!!" para obter o caractere na posição desejada, e em seguida a função "digitToInt" para convertê-lo em inteiro.
-}

{-
12. Um programador especicou a função allDierent para identicar se três números inteiros são
todos diferentes entre si, da seguinte forma:
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p)

(a) O que está errado nessa denição?
(b) Especique corretamente uma função allDierent para o propósito necessário.
-}

{-
13. Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes).
howManyEqual::Int->Int->Int->Int
-}

{-
14.Para o exemplo da função sales::Int->Int dada em sala de aula faça o que se pede:
(a) Implemente a função howManyLess que calcule quantos dias as vendas foram inferiores
a um dado valor, dentro de um intervalo de dias dentro do período total. O primeiro
parâmetro de howManyLess indica o valor mínimo de vendas, o segundo parâmetro indica
o dia do início do intervalo e o terceiro parâmetro é o dia do m do intervalo desejado
dentro do período total de dias da função;
{-parameters: value; interval beginning; interval ending; return value-}
howManyLess::Int->Int->Int->Int
(b) Implemente a função noZeroInPeriod::Int->Bool que retorna True somente se não há
nenhum dia no período em que o número de vendas da função sales foi zero.
(c) Implemente a função zerosInPeriod::[Int] que retorne a lista de todos os dias em que as
vendas foram de zero unidades;
(d) Utilizando listas de inteiros, retorne os dias em que as vendas foram abaixo de um deter-
minado valor passado como parâmetro;
-}

{-
15. A sequencia de Fibonacci é denida e conhecida na literatura. Os dois primeiros números são
0 e 1, e os seguintes são calculados como a soma dos dois anteriores na sequência. Dena a
função antFib que, dado um valor x, calcule a posição de x na sequencia de Fibonacci. Caso x
não esteja na sequência, retorne (-1).
{-exemplo-}
Main> antFib 13 = 7
-}

{-
16. Escreva uma denição equivalente à exibida abaixo, mas usando apenas uma única cláusula
em casamento de padrão:
funny x y z
  | x > z = True
  | y >= x = False
  | otherwise = True
-}

{-
17. Implemente uma função que converte uma letra minúsculas como entrada para seu equiva-
lente em maiúsculo. Caso a entrada não seja uma letra minúscula, retorne o próprio car-
actere de entrada. Como dica, veja a função predenida isLower::Char->Bool. Para veri-
car outras funções pré-denidas para o tipo Char, consulte a biblioteca padrão no endereço
http://zvon.org/other/haskell/Outputglobal/index.html.
-}

{-
18. Defina uma função charToNum::Char->Int que converte um dígito numérico do tipo Char
(como ´3´) para o valor que ele representa em Int, (3). Se o caractere de entrada não representa
um dígito numérico, a função deve retornar -1. Como dica, veja as funções isDigit, chr e ord
do módulo Data.Char.
-}

{-
19. Implemente a função duplicate::String->Int->String que recebe uma string s e um número
inteiro n. A função deve retornar a concatenação de n cópias de s. Se n for zero, retorna "".
Como dica, usar o operador de concatenação pré-denido (++)::String->String->String.
-}

{-
20. Implemente a função pushRight::String->Int->String que recebe uma string s e um número
inteiro n e retorna uma nova string t com k caracteres '>' inseridos no início de s. O valor de
k deve ser tal que o comprimento de t seja igual a n. Obs: se n é menor que o comprimento
de s, a função retorna a própria string s.
{-exemplo-}
Main> pushRight "abc" 5 = ">>abc"
-}

{-
21. Defina um operador binário de nome &-, com a semântica: x &- y = x - 2*y.
 Qual é o resultado da avaliação da expressão 10 &- 3 &- 2, se o operador for definido
como: (a) infixl 6 &-; (b) infixr 6 &-; e (c) infix 6 &- ? Explique esses resultados.
 Qual é o resultado da avaliação da expressão 10 &- 3 * 2, caso o operador seja definido
como: (a) infix 6 &-; e (b) infix 8 &- ? Explique esses resultados.
-}

{-
22. Faça em Haskell uma solução para inverter os elementos de uma lista de Inteiros.
{-exemplo-}
Main> inverte [1,2,3,4,5,6,150] = [150,6,5,4,3,2,1]
-}

{-
23. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de
inteiros onde a primeira conterá os elementos ímpares e a segunda os elementos pares passados
como parâmetro.
{-exemplo-}
Main> separa [1,4,3,4,6,7,9,10] = ([1,3,7,9],[4,4,6,10])
-}

{-
24. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar a string contendo as
letras do alfabeto cuja posição é dada pelos elementos da lista.
{-exemplo-}
Main> converte [1,2,6,1,9] = "ABFAI"
Main> converte [ ] = "".
-}

{-
25. Sabendo que [1..7] é equivalente à lista [1,2,3,4,5,6,7], complete as correspondências abaixo:
(a) ['a'..'g'] =
(b) [0.1 ..0.9] =
(c) [0.1,0.3 .. 0.9] =
(d) [0.1,0.3 ..1.8] =
(e) [0.4,0.2 ..0.8] =
(f) [1,4..15]
-}

{-
26. Faça em Haskell uma solução para o seguinte problema: Dada uma lista de caracteres [Char],
e um caractere a, retornar quantos caracteres da lista são iguais a a.
{-exemplo-}
Main> conta "ABCAABCDDA" "B" = 2
-}

{-
27. Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista
de inteiros ordenada sem elementos repetidos.
{-exemplo-}
Main> purifica [1,1,4,5,5,5,6,7,8,8] = [1,4,5,6,7,8]
-}

{-
28. Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista com uma
repetição de cada elemento de acordo com seu valor.
-}

{-
29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto.
{-exemplo-}
Main> proliferaChar [C,B,D] = "CCCBBDDDD"
-}
