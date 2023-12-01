type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo
pessoa rg
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza",81,'m')
    | rg == 3 = ("Joice Silva",12,'f')
    | rg == 4 = ("Janete Souza",10,'f')
    | rg == 5 = ("Jocileide Strauss",21,'f')
    | otherwise = ("Nao há mais ninguem",0,'x')

menor_idade :: Float -> Meu_tipo
menor_idade x
    | x == 1 = pessoa 1
    | otherwise = menor (pessoa x)(menor_idade(x-1))

menor :: Meu_tipo -> Meu_tipo -> Meu_tipo
menor x y
    |x1 <= x2 = x
    |otherwise = y
        where
        x1 = idade x
        x2 = idade y

idade :: Meu_tipo -> Float
idade (x,y,z) = y

media_idade :: Float -> Float
media_idade x = (soma_idade x) /x

soma_idade :: Float -> Float
soma_idade x
    | x == 1 = idade(pessoa 1)
    | otherwise = idade(pessoa x) + (soma_idade (x-1)) 

ehfeminino :: Meu_tipo -> Float
ehfeminino (x,y,z)
    | z == 'f' = 1
    | otherwise = 0

contaFeminino :: Float -> Float
contaFeminino x 
    | x == 1 = ehfeminino(pessoa 1) 
    | otherwise = ehfeminino(pessoa x) + (contaFeminino(x-1))

type Professor = (Int, String, String, Char)

base :: Int -> (Int, String, String, Char)
base x
    |x == 0 = (1793, "Pedro Paulo", "MESTRE",'M')
    |x == 1 = (1797, "Joana S. Alencar", "MESTRE",'M')
    |x == 2 = (1534, "Joao de Medeiros", "DOUTOR",'F')
    |x == 3 = (1267, "Claudio Cesar de Sá", "DOUTOR",'M')
    |x == 4 = (1737, "Paula de Medeiros", "MESTRE",'F')
    |x == 5 = (1888, "Rita de Matos", "MESTRE",'F')
    |x == 6 = (1356, "Rodolfo Roberto", "DOUTOR", 'M')
    |x == 7 = (1586, "Célia Maria de Sousa", "DOUTOR", 'F')
    |x == 8 = (1800, "Josimar Justino", "MESTRE", 'M')
    |x == 9 = (1698, "Tereza C. Andrade", "MESTRE",'F')
    |x == 10 = ( 0, "" , "" ,'0')

ehDoutor :: Professor -> Int
ehDoutor (m,n,e,s)
    | e == "DOUTOR" = 1
    | otherwise = 0

contaDoutor :: Int -> Int
contaDoutor x
    | x == 0 = ehDoutor(base 0)
    | otherwise = ehDoutor(base x) + (contaDoutor (x-1))

ehMulher :: Professor -> Int
ehMulher (m,n,e,s)
    | s == 'F' = 1
    | otherwise = 0

contaMulher :: Int -> Int
contaMulher x 
    | x == 0 = ehMulher(base 0)
    | otherwise = ehMulher(base x) + (contaMulher(x-1))

ehHomemMestre :: Professor -> Int
ehHomemMestre (m,n,e,s)
    | s == 'M' && e == "DOUTOR" = 1
    | otherwise = 0

contaHomemMestre :: Int -> Int
contaHomemMestre x 
    | x == 0 = ehHomemMestre (base 0)
    | otherwise = ehHomemMestre(base x) + (contaHomemMestre(x-1))

menor_matricula :: Int -> Professor
menor_matricula x
    | x == 1 = base 0
    | otherwise = professorAntigo (base x) (menor_matricula(x-1))

professorAntigo :: Professor -> Professor -> Professor
professorAntigo x y
    |x1 <= x2 = x
    |otherwise = y
        where
        x1 = matricula x
        x2 = matricula y

matricula :: Professor -> Int
matricula (m,n,f,s) = m

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista x









































































