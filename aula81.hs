fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = fatorial (n - 1) * n

potDois :: Int -> Int
potDois 0 = 1
potDois n = 2 * potDois (n - 1)

mul :: Int -> Int -> Int
mul m 0 = 0
mul m n = m + mul m (n - 1)

comp :: [Int] -> Int
comp [] = 0
comp list = 1 + comp (tail list)

(+++) :: [Int] -> [Int] -> [Int]
[] +++ lst2 = lst2
(h:t) +++ lst2  = h : (t +++ lst2)

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (h:t) = reverse' t +++ [h]

-- Exercícios Aula
mulI :: Int -> Int -> Int
mulI m n = if m == n then m else n * mulI m (n - 1)

fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
