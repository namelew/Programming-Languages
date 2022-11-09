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

-- natAdd :: Natural Natural -> Natural
-- natSub :: Natural Natural -> Natural
-- natMul :: Natural Natural -> Natural