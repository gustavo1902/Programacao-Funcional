--Tuplas

{-
1. Defina uma função que retorne uma tupla-3 (tripla) contendo o caractere fornecido com entrada, o mesmo caractere em letras minúsculas ou maiúsculas, e o seu número da tabela ASCII. Exemplo:
converte b (b,B,98)-}

{-
2. Seja o cadastro de pessoas dado pela função a seguir:
pessoa rg |rg == 1 = ("João Silva", 12, 'm')
            |rg == 2 = ("Jonas Souza", 51, 'm')
            ......
            |rg == 321 = ("Jocicleide Strauss", 21, 'f')
            |otherwise = ("Não há ninguém mais", 9999, 'x')
            
Construa funções que retornem os seguintes dados:
(a) O nome da pessoa de menor idade até um determinado registro.
(b) A idade média de todas as pessoas até um dado registro.
(c) O número de pessoas do sexo masculino.
(d) O número do registro da pessoa de maior idade.-}

{-
3. Cosntrua uma função em que, dado um caractere qualquer, retorne uma tupla-3 com o caractere dado, o caractere dado na forma maiúscula/minúscula (o contrário do original) e o número ASCII do original. Exemplo:
Main> analisaLetra 'h'
('h', 'H', 104)
-}

{-
4. Construa uma função em Haskell que recebe 4 inteiros e devolve uma tupla-4 com os quatro valores originais, só que ordenados. Exemplo:
Main> ordena 3 5 1 (-3)
(-3, 1, 3, 5)-}

{-
5. Dadas duas datas (d1, m1, a1) e (d2, m2, a2) tal que data1 <= data2, construa uma função que retorne quantos dias existem entre estas duas datas, onde di define o dia do mês mj no ano ak.-}

{-
6. Crie uma função que receba os coeficientes de uma equação do segundo grau ax²+bx+c=0 na forma (a,b,c) e retorne as raízes desta equação. Trate o caso de raízes imaginárias, indicando um erro.
Exemplo: Main> equacao (1,(-5),6)
         (2,3)-}

{-
7. Construa uma função que dados três dados três valores, verifique se o mesmos podem ser os lados de um triângulo. Se for possível formar o triângulo, retorne uma tupla-2 com o tipo do triângulo formado (com relação às arestas) e o perímetro do mesmo. Exemplo:
Main> triangulo (7,7,11)
("Isóceles",25)-}

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
(d) O nome do prfessor mais antigo (número de menor matricula)-}
