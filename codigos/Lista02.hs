--Tuplas
import Data.Char (toLower, isUpper, toUpper, ord)
import Data.List (minimumBy)

{-
1. Defina uma função que retorne uma tupla-3 (tripla) contendo o caractere fornecido com entrada, o mesmo caractere em letras minúsculas ou maiúsculas, e o seu número da tabela ASCII. Exemplo:
converte b (b,B,98)-}
converte :: Char -> (Char, Char, Int)
converte c = (c, toLower c, fromEnum c)
{-Definmos uma função chamada converte que recebe um caractere como entrada e retorna uma tupla-3 contendo o caractere, o mesmo caractere em letras minúsculas ou maiúsculas e seu número da tabela ASCII. A função toLower é usada para obter a versão em letra minúscula do caractere fornecido, e a função fromEnum é usada para obter o número da tabela ASCII correspondente ao caractere-}

{-
2. Seja o cadastro de pessoas dado pela função a seguir:-
pessoa rg |rg == 1 = ("João Silva", 12, 'm')
            |rg == 2 = ("Jonas Souza", 51, 'm')
            ......
            |rg == 321 = ("Jocicleide Strauss", 21, 'f')
            |otherwise = ("Não há ninguém mais", 9999, 'x')
{--}          
Construa funções que retornem os seguintes dados:
(a) O nome da pessoa de menor idade até um determinado registro.

nomeMenorIdade :: Int -> String
nomeMenorIdade n = fst $ snd $ minimumBy (\x y -> compare (snd x) (snd y)) idades
  where
    idades = map (\(_, (_, idade, _)) -> (idade, (_, idade, _))) $ take n registros

(b) A idade média de todas as pessoas até um dado registro.
idadeMedia :: Int -> Float
idadeMedia n = fromIntegral (sum idades) / fromIntegral (length idades)
  where
    idades = map (\(_, (_, idade, _)) -> idade) $ take n registros

(c) O número de pessoas do sexo masculino.
numHomens :: Int -> Int
numHomens n = length $ filter (\(_, (_, _, sexo)) -> sexo == 'm') $ take n registros

(d) O número do registro da pessoa de maior idade.
numMaiorIdade :: Int -> Int
numMaiorIdade n = fst $ snd $ maximumBy (\x y -> compare (snd x) (snd y)) idades
  where
    idades = map (\(rg, (_, idade, _)) -> (rg, (idade, rg))) $ take n registros

As funções recebem como entrada o número do último registro a ser encontrado.

-}

{-
3. Cosntrua uma função em que, dado um caractere qualquer, retorne uma tupla-3 com o caractere dado, o caractere dado na forma maiúscula/minúscula (o contrário do original) e o número ASCII do original. Exemplo:
Main> analisaLetra 'h'
('h', 'H', 104)
-}
analisaLetra :: Char -> (Char, Char, Int)
analisaLetra c = (c, if isUpper c then toLower c else toUpper c, ord c)


{-
4. Construa uma função em Haskell que recebe 4 inteiros e devolve uma tupla-4 com os quatro valores originais, só que ordenados. Exemplo:
Main> ordena 3 5 1 (-3)
(-3, 1, 3, 5)-}
ordena :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordena a b c d = (min4, midLow, midHigh, max4)
  where
    min4 = minimum [a, b, c, d]
    max4 = maximum [a, b, c, d]
    midLow = minimum [x | x <- [a, b, c, d], x /= min4, x /= max4]
    midHigh = maximum [x | x <- [a, b, c, d], x /= min4, x /= max4]
{-Aqui, utilizamos a função minimum e maximum para encontrar os valores mínimo e máximo da lista formada pelos quatro inteiros. Em seguida, encontramos os valores intermediários, ou seja, aqueles que não são nem o mínimo nem o máximo, e os colocamos nas variáveis midLow e midHigh. Por fim, retornamos uma tupla contendo os quatro valores ordenados.-}
{-
5. Dadas duas datas (d1, m1, a1) e (d2, m2, a2) tal que data1 <= data2, construa uma função que retorne quantos dias existem entre estas duas datas, onde di define o dia do mês mj no ano ak.-}
diasNoAno :: Int -> Int -> Int -> Int
diasNoAno d m a = sum (take (m - 1) diasNoMes) + d + (if ehBissexto a && m > 2 then 1 else 0)
  where
    ehBissexto ano = (ano `mod` 4 == 0 && ano `mod` 100 /= 0) || ano `mod` 400 == 0
    diasNoMes = [31, if ehBissexto a then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

diasEntreDatas :: Int -> Int -> Int -> Int -> Int -> Int -> Int
diasEntreDatas d1 m1 a1 d2 m2 a2
  | a1 == a2 = diasNoAno d2 m2 a2 - diasNoAno d1 m1 a1
  | otherwise = diasNoAno 31 12 a1 - diasNoAno d1 m1 a1 +
                sum [diasNoAno 31 12 i | i <- [a1 + 1 .. a2 - 1]] +
                diasNoAno d2 m2 a2

{-Uma forma de resolver esse problema é calcular o número de dias entre as duas datas. Para isso, podemos calcular o número de dias desde a data inicial até o final do ano, o número de dias entre os anos completos e o número de dias desde o início do ano final até a data final
A função diasNoAno calcula o número de dias desde o início do ano até a data fornecida, levando em conta a quantidade de dias em cada mês e se o ano é bissexto ou não. A função diasEntreDatas utiliza essa função para calcular o número de dias entre as duas datas, somando os dias restantes do ano inicial, os dias entre os anos completos e os dias até a data final.-}
{-
6. Crie uma função que receba os coeficientes de uma equação do segundo grau ax²+bx+c=0 na forma (a,b,c) e retorne as raízes desta equação. Trate o caso de raízes imaginárias, indicando um erro.
Exemplo: Main> equacao (1,(-5),6)
         (2,3)-}
equacao :: (Double, Double, Double) -> (Double, Double)
equacao (a, b, c)
  | delta < 0  = error "Erro: Raízes imaginárias"
  | otherwise  = (x1, x2)
  where
    delta = b^2 - 4*a*c
    x1 = (-b + sqrt delta) / (2*a)
    x2 = (-b - sqrt delta) / (2*a)
{-Explicando o código: a função equacao recebe uma tupla com os coeficientes a, b e c, e utiliza a fórmula de Bhaskara para calcular as raízes x1 e x2 da equação. Caso o discriminante delta seja negativo, a função utiliza a função error para indicar que as raízes são imaginárias. Caso contrário, a função retorna as duas raízes como uma tupla.-}

{-
7. Construa uma função que dados três dados três valores, verifique se o mesmos podem ser os lados de um triângulo. Se for possível formar o triângulo, retorne uma tupla-2 com o tipo do triângulo formado (com relação às arestas) e o perímetro do mesmo. Exemplo:
Main> triangulo (7,7,11)
("Isóceles",25)-}
triangulo :: (Float, Float, Float) -> Maybe (String, Float)
triangulo (a, b, c)
    | a + b > c && a + c > b && b + c > a = Just (tipoTriangulo, perimetro)
    | otherwise = Nothing
  where
    tipoTriangulo
        | a == b && b == c = "Equilátero"
        | a == b || a == c || b == c = "Isóceles"
        | otherwise = "Escaleno"
    perimetro = a + b + c
{-A função triangulo recebe uma tupla com três valores do tipo Float e retorna um valor do tipo Maybe (String, Float), que pode ser Nothing caso não seja possível formar um triângulo com os valores fornecidos ou Just uma tupla com o tipo do triângulo formado (com relação às arestas) e o seu perímetro.

A verificação é feita através de uma condição com guardas. Caso os valores fornecidos possam formar um triângulo, a função tipoTriangulo é chamada para identificar o tipo do triângulo e a variável perimetro é calculada. Caso contrário, a função retorna Nothing.

A função tipoTriangulo é definida através de uma série de condições encadeadas utilizando operadores lógicos. Se todos os lados forem iguais, é um triângulo equilátero; se dois lados forem iguais, é um triângulo isóceles; caso contrário, é um triângulo escaleno.

Para testar a função, basta chamá-la com uma tupla contendo três valores e verificar o resultado retornado, como no exemplo abaixo:-}

{-
8. Apresentada uma base de dados de 10 professores:
base :: Int -> (int, String, String, Char)
base x
    | x == 0 = (1793, "Pedro Paulo",                 "MESTRE", 'M')
    | x == 1 = (1797, "Joana Silva Alencar",         "MESTRE", 'M')
    | x == 2 = (1534, "João De Medeiros",            "DOUTOR", 'F')
    | x == 3 = (1267, "Cláudio César de Sá",         "DOUTOR", 'M')
    | x == 4 = (1737, "Paula de Medeiros",           "MESTRE", 'F')
    | x == 5 = (1888, "Rita de Matos",               "MESTRE", 'F')
    | x == 9 = (1698, "Tereza Cristina Andrade",     "MESTRE", 'F')
    | x == 10 = (0,   "",                            "", '0')
    
Construa funções que retornem:
(a) O número de doutores na base.
(b) O número de milhares.
(c) O número de mestres do sexo masculino.
(d) O nome do professor mais antigo (número de menor matricula)-}


type Matricula = Int
type Nome = String
type Sobrenome = String
type Disciplina = Char
type Professor = (Matricula, Nome, Sobrenome, Disciplina)

base :: Int -> Professor
base 0 = (0, "Sem", "Professor", 'A')
base 1 = (1, "João", "Silva", 'B')
base 2 = (2, "Maria", "Santos", 'C')
base 3 = (3, "José", "Oliveira", 'D')
base 4 = (4, "Ana", "Souza", 'E')
base 5 = (5, "Carlos", "Ferreira", 'F')
base 6 = (6, "Mariana", "Gomes", 'G')
base 7 = (7, "Paulo", "Costa", 'H')
base 8 = (8, "Juliana", "Rocha", 'I')
base 9 = (9, "Lucas", "Albuquerque", 'J')
base 10 = (10, "Bianca", "Almeida", 'K')
base _ = (0, "Sem", "Professor", 'A')

matriculaProfessorMaisAntigo :: [Professor] -> Matricula
matriculaProfessorMaisAntigo = fst . minimumBy (\(matricula1, _, _, _) (matricula2, _, _, _) -> compare matricula1 matricula2)

nomeProfessorMaisAntigo :: [Professor] -> Nome
nomeProfessorMaisAntigo ps = case filter (\(matricula, _, _, _) -> matricula == matriculaMaisAntigo) ps of
                               [(m, n, _, _)] -> n
                               _ -> "Sem Professor"
  where matriculaMaisAntigo = matriculaProfessorMaisAntigo ps

main :: IO ()
main = do
  putStrLn $ "Professor mais antigo: " ++ nomeProfessorMaisAntigo (filter (\(matricula, _, _, _) -> matricula /= 0) [base x | x <- [0 .. 10]])

{-Erro minimumBy-}




