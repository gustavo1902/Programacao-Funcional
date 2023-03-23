module Main (main) where

main :: IO ()
main = do
    let pessoa = Pessoa Estagiario "Joao"
    print (verSalario pessoa)

data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show

data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Pessoa Estagiario _) = 1000
verSalario (Pessoa Programador _) = 2000
verSalario (Pessoa Coordenador _) = 3000
verSalario (Pessoa Gerente _) = 4000

verFolha :: Pessoa -> String
verFolha p = "{nome: \"" ++ (nome p)++ "\", cargo: \"" ++ (show $ cargo p) ++ "\", salario: " ++ (show $ verSalario p) ++ "}"

promover :: Pessoa -> Pessoa
promover (Pessoa Estagiario n) = Pessoa Programador n
promover (Pessoa Programador n) = Pessoa Coordenador n
promover (Pessoa Coordenador n) = Pessoa Gerente n
promover pessoa@(Pessoa Gerente _) = pessoa
