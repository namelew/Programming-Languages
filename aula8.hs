quadrado :: Int -> Int
quadrado x = 4*(x + x)

dquadrado :: Int -> Int
dquadrado x = 2*(4*x)

mediaN x y z = (x + y + z)/3

kwattRes salario consumo = let quilo = (consumo * (salario/5)) in
                            quilo - (quilo * 0.15)