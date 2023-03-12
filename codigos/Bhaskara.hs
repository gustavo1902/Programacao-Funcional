-- Define uma função para calcular o discriminante
discriminant :: Float -> Float -> Float -> Float
discriminant a b c = b*b - 4*a*c

-- Define a função principal que resolve a equação de segundo grau
quadraticFormula :: Float -> Float -> Float -> [Float]
quadraticFormula a b c
    | a == 0    = error "Não é uma equação de segundo grau!"
    | delta < 0 = []
    | otherwise = [(-b + sqrt delta) / (2*a), (-b - sqrt delta) / (2*a)]
    where delta = discriminant a b c
quadraticFormula 1 (-3) 2 -- retorna [2.0,1.0]
