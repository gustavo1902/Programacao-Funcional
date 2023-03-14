module Main where

data Jogada = Pedra | Papel | Tesoura
  deriving (Eq, Show)

data Resultado = Vitoria | Derrota | Empate
  deriving (Eq, Show)

jogar :: Jogada -> Jogada -> Resultado
jogar Pedra Pedra = Empate
jogar Pedra Papel = Derrota
jogar Pedra Tesoura = Vitoria
jogar Papel Pedra = Vitoria
jogar Papel Papel = Empate
jogar Papel Tesoura = Derrota
jogar Tesoura Pedra = Derrota
jogar Tesoura Papel = Vitoria
jogar Tesoura Tesoura = Empate
