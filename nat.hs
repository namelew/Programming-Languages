data Natural = Zero
            |  Suc [Natural]
            deriving Show

nat2integer'' :: Natural -> Int
nat2integer'' (Zero) = 0
nat2integer'' (Suc [(Zero)]) = 1
nat2integer'' (Suc [(Suc [(Zero)])]) = 2
nat2integer'' (Suc [(Suc [(Suc [(Zero)])])]) = 3
nat2integer'' (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = 4

integer2nat'' :: Int -> Natural
integer2nat'' 0 = (Zero)
integer2nat'' 1 = (Suc [(Zero)])
integer2nat'' 2 = (Suc [(Suc [(Zero)])])
integer2nat'' 3 = (Suc [(Suc [(Suc [(Zero)])])])
integer2nat'' 4 = (Suc [(Suc [(Suc [(Suc [(Zero)])])])])

natAdd :: Natural -> Natural -> Natural
natAdd n1 n2 = integer2nat'' ((nat2integer'' n1 + nat2integer'' n2) `mod` 4)

natSub :: Natural -> Natural -> Natural
natSub n1 n2 = integer2nat'' ((nat2integer'' n1 - nat2integer'' n2) `mod` 4)

natMul :: Natural -> Natural -> Natural
natMul n1 n2 = integer2nat'' ((nat2integer'' n1 - nat2integer'' n2) `mod` 4)