import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

type Cor = String
type Preco = Double
type Tinta = (Cor, Preco)
type Estoque = [Tinta]

selecionarTintaMaisBarata :: Estoque -> Tinta
selecionarTintaMaisBarata = minimumBy (comparing snd)

selecionarTintaMaisCara :: Estoque -> Tinta
selecionarTintaMaisCara = maximumBy (comparing snd)

estoque :: Estoque
estoque = [("azul", 10.0), ("vermelho", 8.0), ("amarelo", 12.0)]

main :: IO ()
main = do
  let tintaMaisBarata = selecionarTintaMaisBarata estoque
      tintaMaisCara = selecionarTintaMaisCara estoque
  putStrLn $ "A tinta mais barata é " ++ show tintaMaisBarata
  putStrLn $ "A tinta mais cara é " ++ show tintaMaisCara
