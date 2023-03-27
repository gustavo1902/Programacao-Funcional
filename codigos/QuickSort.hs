qsort :: (Ord a) => [a] -> [a]
-- Tratamento para lista vazia
qsort [] = []
-- Verificando a existência de apenas um elemento, se for apenas um a lista já estará ordenada
qsort [x] = [x]
-- Quicksort para listas com mais de um elemento
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]
{-x:xs é usado para decompor a lista em um primeiro elemento x e uma lista restante xs. O elemento x é então escolhido como pivô. Após são criadas duas listas:
qsort[a | a <- xs, a <= x] contendo todos os elementos de xs menores ou iguais a x.
qsort[a | a <- xs, a > x] contendo todos os elementos de xs que são maiores que x-}

lista = [(1), (2), (7), (5), (8)]

main = do
  print (qsort lista)
