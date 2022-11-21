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
natAdd (Zero) n2 = n2 -- 0 + n
natAdd n1 (Zero) = n1 -- n + 0
natAdd (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) n2 = n2 -- 4 + n
natAdd n1 (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = n1 -- n + 4
natAdd (Suc [(Suc [(Zero)])]) (Suc [(Suc [(Zero)])]) = (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) -- 2 + 2
natAdd (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Suc [(Zero)])]) = (Suc [(Zero)]) -- 3 + 2
natAdd (Suc [(Suc [(Zero)])]) (Suc [(Suc [(Suc [(Zero)])])]) = (Suc [(Zero)]) -- 2 + 3
natAdd (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Suc [(Suc [(Zero)])])]) = (Suc [(Suc [(Zero)])]) -- 3 + 3
natAdd (Suc [(Zero)]) n2 = (Suc [n2]) -- 1 + n
natAdd n1 (Suc [(Zero)]) = (Suc [n1]) -- n + 1

natSub :: Natural -> Natural -> Natural
natSub (Zero) n2 = n2 -- 0 - n
natSub n1 (Zero) = n1 -- n - 0
natSub (Suc [(Zero)]) (Suc [(Zero)]) = (Zero) -- n - n
natSub (Suc [(Suc [(Zero)])]) (Suc [(Suc [(Zero)])]) = (Zero) -- n - n
natSub (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Suc [(Suc [(Zero)])])]) = (Zero) -- n - n
natSub (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = (Zero) -- n - n
natSub (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) (Suc [(Zero)]) = (Suc [(Suc [(Suc [(Zero)])])]) -- 4 - n
natSub (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) (Suc [(Suc [(Zero)])]) = (Suc [(Suc [(Zero)])]) -- 4 - n
natSub (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) (Suc [(Suc [(Suc [(Zero)])])]) = (Suc [(Zero)]) -- 4 - n
natSub (Suc [(Zero)]) (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = (Suc [(Suc [(Suc [(Zero)])])]) -- n - 4
natSub (Suc [(Suc [(Zero)])]) (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = (Suc [(Suc [(Zero)])]) -- n - 4
natSub (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Suc [(Suc [(Suc [(Zero)])])])]) = (Suc [(Zero)]) -- n - 4
natSub (Suc [(Zero)]) (Suc [(Suc [(Zero)])]) = (Suc [(Zero)]) -- 1 - n
natSub (Suc [(Zero)]) (Suc [(Suc [(Suc [(Zero)])])]) = (Suc [(Suc [(Zero)])]) -- 1 - n
natSub (Suc [(Suc [(Zero)])]) (Suc [(Zero)]) = (Suc [(Zero)]) -- n - 1
natSub (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Zero)]) = (Suc [(Suc [(Zero)])]) -- n - 1
natSub (Suc [(Suc [(Suc [(Zero)])])]) (Suc [(Suc [(Zero)])]) = (Suc [(Zero)]) -- 3 - 2
natSub (Suc [(Suc [(Zero)])]) (Suc [(Suc [(Suc [(Zero)])])]) = (Suc [(Zero)]) -- 2 - 3 

natMul :: Natural -> Natural -> Natural
natMul m (Zero) = (Zero)
natMul m n = let m natAdd natMul m natSub n (Suc [(Zero)]) -- adicionar let e wheres