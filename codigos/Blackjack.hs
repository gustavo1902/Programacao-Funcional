-- Definir um tipo de dados algébricos para representar um jogo de cartas
data Carta = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei
data Naipe = Copas | Ouros | Espadas | Paus
data JogoDeCartas = Baralho [Carta] | Mao [Carta]

-- Função que embaralha um baralho de cartas
embaralhar :: [Carta] -> [Carta]
embaralhar [] = []
embaralhar xs = let (ys,zs) = divide xs in intercalar (embaralhar ys) (embaralhar zs)

-- Funções auxiliares para a função embaralhar
divide :: [a] -> ([a],[a])
divide [] = ([],[])
divide [x] = ([x],[])
divide (x:y:zs) = let (xs,ys) = divide zs in (x:xs,y:ys)

intercalar :: [a] -> [a] -> [a]
intercalar xs [] = xs
intercalar [] ys = ys
intercalar (x:xs) (y:ys) = x : y : intercalar xs ys

-- Função que ordena uma mão de cartas
ordenarMao :: [Carta] -> [Carta]
ordenarMao [] = []
ordenarMao (x:xs) = let menores = [y | y <- xs, y <= x]
                        maiores = [y | y <- xs, y > x]
                    in ordenarMao menores ++ [x] ++ ordenarMao maiores

-- Função que calcula o valor de uma mão de blackjack
valorMao :: [Carta] -> Int
valorMao mao = let maoOrdenada = ordenarMao mao
               in valorMaoAux maoOrdenada 0

valorMaoAux :: [Carta] -> Int -> Int
valorMaoAux [] total = total
valorMaoAux (As:xs) total
    | total + 11 > 21 = valorMaoAux xs (total + 1)
    | otherwise = valorMaoAux xs (total + 11)
valorMaoAux (Dois:xs) total = valorMaoAux xs (total + 2)
valorMaoAux (Tres:xs) total = valorMaoAux xs (total + 3)
valorMaoAux (Quatro:xs) total = valorMaoAux xs (total + 4)
valorMaoAux (Cinco:xs) total = valorMaoAux xs (total + 5)
valorMaoAux (Seis:xs) total = valorMaoAux xs (total + 6)
valorMaoAux (Sete:xs) total = valorMaoAux xs (total + 7)
valorMaoAux (Oito:xs) total = valorMaoAux xs (total + 8)
valorMaoAux (Nove:xs) total = valorMaoAux xs (total + 9)
valorMaoAux (Dez:xs) total = valorMaoAux xs (total + 10)
valorMaoAux (Valete:xs) total = valorMaoAux xs (total + 10)
valorMaoAux (Dama:xs) total = valorMaoAux xs (total + 10)
valorMaoAux (Rei:xs) total = valorMaoAux xs (total + 10)

-- Função que verifica se uma mão de blackjack é um blackjack (21 pontos com apenas 2 cartas)
ehBlackjack :: [Carta] -> Bool
ehBlackjack mao = length mao == 2 && valorMao mao == 21

-- Função que simula uma jogada de blackjack
jogarBlackjack :: [Carta] -> [Carta] -> IO ()
jogarBlackjack baralho jogador = do
let jogadorAtualizado = adicionarCarta baralho jogador
let valorJogador = valorMao jogadorAtualizado
if valorJogador > 21 then do
putStrLn "Você perdeu!"
else if valorJogador == 21 then do
putStrLn "Você ganhou!"
else do
putStrLn ("Sua mão: " ++ mostrarMao jogadorAtualizado)
putStrLn "Deseja pegar outra carta? (s/n)"
opcao <- getLine
if opcao == "s" then jogarBlackjack baralho jogadorAtualizado
else do
let baralhoAtualizado = drop (length jogadorAtualizado - length jogador) baralho
let dealer = adicionarCarta baralhoAtualizado []
let valorDealer = valorMao dealer
putStrLn ("Mão do dealer: " ++ mostrarMao dealer)
if valorDealer > 21 then do
putStrLn "Você ganhou!"
else if valorJogador > valorDealer then do
putStrLn "Você ganhou!"
else do
putStrLn "Você perdeu!"

-- Funções auxiliares para a função jogarBlackjack
adicionarCarta :: [Carta] -> [Carta] -> [Carta]
adicionarCarta [] mao = mao
adicionarCarta (x:xs) mao = if valorMao (x:mao) > 21 then mao
else adicionarCarta xs (x:mao)

mostrarCarta :: Carta -> String
mostrarCarta As = "Ás"
mostrarCarta Dois = "Dois"
mostrarCarta Tres = "Três"
mostrarCarta Quatro = "Quatro"
mostrarCarta Cinco = "Cinco"
mostrarCarta Seis = "Seis"
mostrarCarta Sete = "Sete"
mostrarCarta Oito = "Oito"
mostrarCarta Nove = "Nove"
mostrarCarta Dez = "Dez"
mostrarCarta Valete = "Valete"
mostrarCarta Dama = "Dama"
mostrarCarta Rei = "Rei"

mostrarNaipe :: Naipe -> String
mostrarNaipe Copas = "Copas"
mostrarNaipe Ouros = "Ouros"
mostrarNaipe Espadas = "Espadas"
mostrarNaipe Paus = "Paus"

mostrarMao :: [Carta] -> String
mostrarMao [] = ""
mostrarMao [x] = mostrarCarta x ++ " de " ++ mostrarNaipe (naipe x)
mostrarMao (x:xs) = mostrarCarta x ++ " de " ++ mostrarNaipe (naipe x) ++ ", " ++ mostrarMao xs

-- Função que embaralha um baralho
embaralhar :: [Carta] -> [Carta]
embaralhar baralho = shuffle baralho (length baralho)

-- Função que embaralha um baralho usando o algoritmo de Fisher-Yates
shuffle :: [Carta] -> Int -> [Carta]
shuffle [] _ = []
shuffle (x:xs) n = let k = n mod (length (x:xs))
in (x:shuffle (take k xs ++ drop (k+1) (x:xs)) (n-1))

-- Baralho de teste com 3 cartas para verificar o funcionamento da função valorMao
baralhoTeste = [As, Dois, Dez]

-- Chamada da função jogarBlackjack com um baralho de 52 cartas embaralhado
main :: IO ()
main = do
let baralho = embaralhar baralhoCompleto
let jogador = [head baralho, head (tail baralho)]
jogarBlackjack baralho jogador
