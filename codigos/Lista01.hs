import Data.Char (digitToInt) --Exercício 11
import Data.Char (isLower) --Exercício 17
import Data.Char (isDigit, ord) --Exercício 18 e 29
import Prelude --Exercício 23
import Data.List
import Data.List (nub) -- Exercício 27


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
2. Localize, explique e corrija o erro na função que deve calcular o fatorial de um número, como
se segue:
fat::Int->Int
fat x = x * fat(x-1)
-}
{- Com a implementação de uma claúsula de parada-}
fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x - 1)

{-
3. Considere a função em Haskell soma::Int->Int->Int que retorna a soma entre os dois parâmet-
ros. Assim, faça uma função em Haskell que resulte a multiplicação de dois parâmetros fazendo
uso da função soma.
-}

soma :: Int -> Int -> Int 
soma x y = x + y

mult :: Int -> Int -> Int
mult x y
  | y == 0    = 0
  | otherwise = soma x (mult x (y-1))


{-
4. Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um número inteiro.
Main> invertInt 123 = 321
-}

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

{-
5. Escreva, em Haskell, a denição de uma função fourPower que retorne o seu argumento elevado
à quarta potência. Use a função square dada em sala de aula na denição de fourPower.
fourPower :: Int -> Int
-}

square :: Num a => a -> a
square x = x * x

fourPower :: Num a => a -> a
fourPower x = square (square x)

{-
6. Sequência infinita de subtrações de raizes de 6-}

sqrtSeq :: Int -> Double
sqrtSeq 0 = sqrt 6
sqrtSeq i = sqrtSeq (i-1) + sqrt 6

{-
7. Escreva, em Haskell, uma função que informa de quantas maneiras é possível escolher n objetos
em uma coleção original de m objetos, para m ≥ n.
-}
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

{-
10. Escreva, em Haskell, uma função que retorna o último dígito de um número inteiro.
lastDigit 1234 = 4
-}

ultimoDigito :: Int -> Int
ultimoDigito x = read [last(show x)]
{-Aqui é convertido o número em string com a função show e a função last retorna o último caractere, com read converte de volta para inteiro-}

{-
ultimoDigito :: Int -> Int
ultimoDigito n = n `mod` 10

aqui o operador % retorna o resto da divisão de n por 10 que será o último digito
-}

{-
11. Escreva, em Haskell, uma função que retorna o dígito de um número inteiro de acordo com a
posição informada.
anyDigit 0 7689 = 7
anyDigit 2 7689 = 8
anyDigit 9 7689 = -1
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
12. Um programador especificou a função allDifierent para identificar se três números inteiros são
todos diferentes entre si, da seguinte forma:
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p)

(a) O que está errado nessa definenição?
(b) Especifique corretamente uma função allDifferent para o propósito necessário.
-}

{-
a) O problema com a definição da função allDifferent é que ela só verifica a diferença entre os pares (m,n) e (n,p), mas não verifica a diferença entre os pares (m,p) e (p,n). Isso significa que a função não é capaz de detectar quando todos os três números são iguais.

b) Uma possível definição correta para a função allDifferent seria:-}
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)
{-
Esta função verifica se cada um dos pares (m,n), (n,p) e (m,p) é diferente, garantindo que todos os três números são diferentes entre si.-}


{-
13. Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes).
howManyEqual::Int->Int->Int->Int
-}
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && b == c = 3  -- se todos forem iguais, retorna 3
  | a == b || b == c || a == c = 2  -- se pelo menos dois forem iguais, retorna 2
  | otherwise = 0  -- se nenhum for igual, retorna 0
{-
Explicação:

A função recebe três números inteiros a, b e c como argumentos e usa padrões de correspondência para verificar se eles são iguais entre si. Se os três forem iguais, a função retorna 3, indicando que todos são iguais. Se pelo menos dois forem iguais, a função retorna 2, indicando que dois são iguais e o terceiro é diferente. Caso contrário, se nenhum dos três for igual ao outro, a função retorna 0, indicando que todos são diferentes.
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
-- Definição da função sales dada em sala de aula
sales :: Int -> Int
sales day
  | day < 1 = error "Dia inválido"
  | day == 1 = 5
  | day == 2 = 3
  | day == 3 = 8
  | day == 4 = 2
  | day == 5 = 6
  | day == 6 = 1
  | otherwise = sales (day-6) + sales (day-5) + sales (day-4) + sales (day-3) + sales (day-2) + sales (day-1)

-- (a) Implementação da função howManyLess
howManyLess :: Int -> Int -> Int -> Int
howManyLess value start end = length $ filter (<value) [sales day | day <- [start..end]]

-- (b) Implementação da função noZeroInPeriod
noZeroInPeriod :: Int -> Bool
noZeroInPeriod days = all (/= 0) [sales day | day <- [1..days]]

-- (c) Implementação da função zerosInPeriod
zerosInPeriod :: [Int]
zerosInPeriod = [day | day <- [1..31], sales day == 0]

-- (d) Implementação da função belowValueInPeriod
belowValueInPeriod :: Int -> [Int]
belowValueInPeriod value = [day | day <- [1..31], sales day < value]

{-
Explicação:

(a) A função howManyLess recebe três parâmetros: o valor mínimo de vendas desejado (value), o dia inicial do intervalo de dias (start) e o dia final do intervalo de dias (end). A função usa uma list comprehension para gerar uma lista de vendas para cada dia no intervalo e, em seguida, usa a função filter para manter apenas os valores menores que o valor mínimo de vendas desejado. O resultado é o comprimento dessa lista filtrada, que indica quantos dias as vendas foram inferiores ao valor mínimo.

(b) A função noZeroInPeriod recebe um parâmetro days, que indica o número total de dias do período. A função usa uma list comprehension para gerar uma lista de vendas para cada dia no período e, em seguida, usa a função all para verificar se todos os elementos da lista são diferentes de zero. Se todos os elementos da lista forem diferentes de zero, a função retorna True, indicando que não há nenhum dia no período em que o número de vendas foi zero.

(c) A função zerosInPeriod não recebe nenhum parâmetro. A função usa uma list comprehension para gerar uma lista de dias em que as vendas foram zero, verificando se o valor de vendas para cada dia é igual a zero.

(d) A função belowValueInPeriod recebe um parâmetro value, que indica o valor mínimo de vendas desejado. A função usa uma list comprehension para gerar uma lista de dias em que as vendas foram abaixo desse valor mínimo, verificando se o valor de vendas para cada dia é menor que o valor mínimo desejado.-}

{-
15. A sequencia de Fibonacci é definida e conhecida na literatura. Os dois primeiros números são
0 e 1, e os seguintes são calculados como a soma dos dois anteriores na sequência. Defina a
função antFib que, dado um valor x, calcule a posição de x na sequencia de Fibonacci. Caso x
não esteja na sequência, retorne (-1).
{-exemplo-}
Main> antFib 13 = 7
-}
{-
Para resolver esse problema, podemos usar um laço para gerar a sequência de Fibonacci até encontrarmos o número desejado ou até ultrapassá-lo. Vamos definir a função antFib:-}
antFib :: Int -> Int
antFib x = antFibAux x 0 1 1
    where antFibAux x ant1 ant2 pos
            | x == ant2 = pos
            | ant2 > x = -1
            | otherwise = antFibAux x ant2 (ant1 + ant2) (pos + 1)

{-
Para resolver esse problema, podemos usar um laço para gerar a sequência de Fibonacci até encontrarmos o número desejado ou até ultrapassá-lo. Vamos definir a função antFib:

haskell
Copy code
antFib :: Int -> Int
antFib x = antFibAux x 0 1 1
    where antFibAux x ant1 ant2 pos
            | x == ant2 = pos
            | ant2 > x = -1
            | otherwise = antFibAux x ant2 (ant1 + ant2) (pos + 1)
Explicando a função:

A função principal antFib recebe um número x e chama a função auxiliar antFibAux com os parâmetros iniciais para a sequência de Fibonacci (0 e 1), e a posição atual como 1.
A função antFibAux recebe o número x a ser buscado, o antecessor (ant1) e o atual (ant2) números da sequência de Fibonacci e a posição atual (pos).
Se o número x for encontrado (x == ant2), retornamos a posição atual (pos).
Se o número atual (ant2) ultrapassar x, significa que x não está na sequência, então retornamos -1.
Caso contrário, chamamos a função novamente com os próximos números da sequência e a próxima posição.-}

{-
16. Escreva uma denição equivalente à exibida abaixo, mas usando apenas uma única cláusula
em casamento de padrão:
funny x y z
  | x > z = True
  | y >= x = False
  | otherwise = True
-}
{-Podemos reescrever a função funny usando uma única cláusula em casamento de padrão, utilizando a expressão otherwise como guarda:-}
funny x y z
  | x > z || y < x = True
  | otherwise = False
{-Nessa definição, a primeira condição x > z || y < x é avaliada primeiro. Se essa condição for verdadeira, a função retorna True. Caso contrário, a expressão otherwise é avaliada, que é verdadeira somente quando nenhuma das outras condições anteriores é satisfeita. Nesse caso, a função retorna False. Note que essa definição é equivalente à definição original da função funny.-}

{-
17. Implemente uma função que converte uma letra minúsculas como entrada para seu equiva-
lente em maiúsculo. Caso a entrada não seja uma letra minúscula, retorne o próprio car-
actere de entrada. Como dica, veja a função predefinida isLower::Char->Bool. Para veri-
ficar outras funções pré-definidas para o tipo Char, consulte a biblioteca padrão no endereço
http://zvon.org/other/haskell/Outputglobal/index.html.
-}
toUpper :: Char -> Char
toUpper c
  | isLower c = toEnum (fromEnum c - 32)
  | otherwise = c

{-
Na primeira linha da definição, declaramos o nome da função e o tipo de seus parâmetros e do valor de retorno. Na segunda linha, utilizamos uma cláusula em casamento de padrão para tratar a entrada c. Se c for uma letra minúscula, a função isLower retorna True, e então aplicamos a fórmula toEnum (fromEnum c - 32) para converter a letra minúscula para maiúscula. Se c não for uma letra minúscula, a expressão otherwise é avaliada e a própria entrada c é retornada.

Note que na fórmula toEnum (fromEnum c - 32), a função fromEnum converte o caractere c em um número inteiro correspondente ao seu código ASCII, subtraímos 32 desse número para obter o código ASCII correspondente à letra maiúscula equivalente e, em seguida, aplicamos a função toEnum para converter o código ASCII em um caractere correspondente.
-}

{-
18. Defina uma função charToNum::Char->Int que converte um dígito numérico do tipo Char
(como ´3´) para o valor que ele representa em Int, (3). Se o caractere de entrada não representa
um dígito numérico, a função deve retornar -1. Como dica, veja as funções isDigit, chr e ord
do módulo Data.Char.
-}

charToNum :: Char -> Int
charToNum c
  | isDigit c = ord c - ord '0'
  | otherwise = -1
{-
Explicação:

A função isDigit verifica se o caractere c é um dígito numérico.
A função ord retorna o código ASCII do caractere c.
Subtraindo o código ASCII do caractere '0' do código ASCII do caractere c, obtemos o valor inteiro correspondente ao dígito numérico.
Se o caractere c não for um dígito numérico, retornamos -1.-}

{-
19. Implemente a função duplicate::String->Int->String que recebe uma string s e um número
inteiro n. A função deve retornar a concatenação de n cópias de s. Se n for zero, retorna "".
Como dica, usar o operador de concatenação pré-denido (++)::String->String->String.
-}
duplicate :: String -> Int -> String
duplicate s n
  | n == 0 = ""
  | otherwise = concat (replicate n s)

{-
Explicação:

A função duplicate recebe uma string s e um número inteiro n.
Se n for igual a zero, a função retorna uma string vazia "".
Caso contrário, a função usa a função replicate para criar uma lista de n cópias da string s e, em seguida, usa a função concat para concatenar essas cópias em uma única string.-}

{-
20. Implemente a função pushRight::String->Int->String que recebe uma string s e um número
inteiro n e retorna uma nova string t com k caracteres '>' inseridos no início de s. O valor de
k deve ser tal que o comprimento de t seja igual a n. Obs: se n é menor que o comprimento
de s, a função retorna a própria string s.
{-exemplo-}
Main> pushRight "abc" 5 = ">>abc"
-}
pushRight :: String -> Int -> String
pushRight s n
  | length s >= n = s
  | otherwise = replicate (n - length s) '>' ++ s
{-
Explicação:

Se o comprimento da string s for maior ou igual a n, a função simplesmente retorna s sem fazer nada.
Caso contrário, a função cria uma nova string com k caracteres '>', onde k é a diferença entre n e o comprimento de s, e concatena essa nova string com s.-}

{-
21. Defina um operador binário de nome &-, com a semântica: x &- y = x - 2*y.
 Qual é o resultado da avaliação da expressão 10 &- 3 &- 2, se o operador for definido
como: (a) infixl 6 &-; (b) infixr 6 &-; e (c) infix 6 &- ? Explique esses resultados.
 Qual é o resultado da avaliação da expressão 10 &- 3 * 2, caso o operador seja definido
como: (a) infix 6 &-; e (b) infix 8 &- ? Explique esses resultados.
-}

{-
-}

{-
22. Faça em Haskell uma solução para inverter os elementos de uma lista de Inteiros.
{-exemplo-}
Main> inverte [1,2,3,4,5,6,150] = [150,6,5,4,3,2,1]
-}
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]
{-Explicação: A função inverte recebe uma lista de inteiros como entrada e retorna uma nova lista com os elementos invertidos. A definição usa pattern matching para tratar o caso base da recursão, quando a lista é vazia, e o caso recursivo, quando a lista tem pelo menos um elemento.
29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto.
{-exemplo-}
Main> proliferaChar [C,B,D] = "CCCBBDDDD"
No caso recursivo, a função chama a si mesma para processar a lista xs e, em seguida, concatena a lista invertida com o elemento x (que é colocado em uma lista de um único elemento usando a sintaxe [x]).-}
{-
23. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar uma dupla de listas de
inteiros onde a primeira conterá os elementos ímpares e a segunda os elementos pares passados
como parâmetro.
{-exemplo-}
Main> separa [1,4,3,4,6,7,9,10] = ([1,3,7,9],[4,4,6,10])
-}
separa :: [Int] -> ([Int], [Int])
separa xs = partition (not . even) xs


{-
24. Faça em Haskell uma solução para, dada uma lista de inteiros, retornar a string contendo as
letras do alfabeto cuja posição é dada pelos elementos da lista.
{-exemplo-}
Main> converte [1,2,6,1,9] = "ABFAI"
Main> converte [ ] = "".
-}
converte :: [Int] -> String
converte xs = [alphabet !! (x-1) | x <- xs, x <= length alphabet]
    where alphabet = ['A'..'Z']

{-Explicação:
Uma possível solução seria criar uma lista com todas as letras do alfabeto, e então usar a função !! para obter as letras correspondentes às posições presentes na lista de inteiros. Se uma posição for maior que o tamanho da lista de letras, basta ignorá-la.

Explicação da função:

alphabet = ['A'..'Z']: cria uma lista com todas as letras do alfabeto maiúsculas;
xs é a lista de inteiros passada como parâmetro;
[alphabet !! (x-1) | x <- xs, x <= length alphabet] é uma lista construída com base na lista xs. Para cada elemento x em xs, se x for menor ou igual ao tamanho de alphabet, a letra correspondente é adicionada à lista final usando o operador !!, que retorna o elemento na posição dada. Note que a posição é obtida subtraindo-se 1 de x, pois as posições em Haskell começam em 0, mas queremos usar a posição 1 para a letra A, 2 para a letra B, e assim por diante.
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
conta :: [Char] -> Char -> Int
conta xs a = length (filter (== a) xs)
{-Para resolver o problema, podemos utilizar a função filter para obter uma lista contendo apenas os caracteres que são iguais a a, e em seguida aplicar a função length para obter o tamanho dessa lista
-}

{-
27. Para uma lista de elementos inteiros ordenada qualquer, faça uma função que retorne uma lista
de inteiros ordenada sem elementos repetidos.
{-exemplo-}
Main> purifica [1,1,4,5,5,5,6,7,8,8] = [1,4,5,6,7,8]
-}
purifica :: Ord a => [a] -> [a]
purifica = nub
{-
Uma maneira de implementar essa função é utilizando a função nub da biblioteca padrão Data.List, que remove elementos repetidos de uma lista. Como a lista já está ordenada, não precisamos nos preocupar em ordená-la novamente após remover os elementos repetidos.
Dessa forma, a função purifica retorna a lista de inteiros ordenada sem elementos repetidos.
-}

{-
28. Faça uma solução em Haskell que, dada uma lista de inteiros, ela retorne uma lista com uma
repetição de cada elemento de acordo com seu valor.
-}
repeteLista :: [Int] -> [Int]
repeteLista xs = concatMap (\x -> replicate x x) xs
{-
Explicação: A função replicate x x cria uma lista com x elementos iguais a x. Por exemplo, replicate 3 2 retorna [2,2,2]. A função concatMap aplica a função replicate x x para cada elemento x da lista de entrada xs, e concatena todas as listas resultantes em uma única lista.
-}

{-
29. Faça uma solução em Haskell que, dada uma lista de caracteres maiúsculos, ela retorne uma
lista com uma repetição de cada elemento de acordo com o valor de sua ordem no alfabeto.
{-exemplo-}
Main> proliferaChar [C,B,D] = "CCCBBDDDD"
-}
proliferaChar :: String -> String
proliferaChar xs = concat [replicate (ord c - ord 'A' + 1) c | c <- xs]
{-
Para resolver este problema, podemos utilizar a função ord do módulo Data.Char para obter o valor numérico de cada caractere em relação ao seu lugar na tabela ASCII. Em seguida, subtraímos o valor de 'A' para obter o índice correspondente a cada letra no alfabeto. Então, usamos uma lista de compreensão para criar a nova lista com a repetição de cada letra de acordo com seu índice.

Na primeira linha da função, importamos o módulo Data.Char e a função ord. Na segunda linha, usamos uma lista de compreensão para criar uma lista com as repetições de cada letra. Para cada caractere c na lista de entrada xs, usamos a função replicate para criar uma lista com ord c - ord 'A' + 1 cópias do caractere c. Por fim, concatenamos as sublistas geradas para criar a lista final.-}
