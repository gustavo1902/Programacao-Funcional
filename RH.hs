module Main (main) where
main :: IO ()

data Cargo = Estagiario | Programador | Coordenador |Gerente deriving Show

data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Pessoa Estagiario _) = 1000
verSalario (Pessoa Programador _) = 2000
verSalario (Pessoa Coordenador _) = 3000
verSalario (Pessoa Gerente _) = 4000

main = do
    let pessoa = Pessoa Estagiario "Joao"
    print (verSalario pessoa)
