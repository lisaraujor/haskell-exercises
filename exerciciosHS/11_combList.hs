aux :: Int -> [Int]
aux num = [0..num-1]

combineList :: Int -> Int -> [[Int]]
combineList n 1 = [[n-1]..[0]]
combineList n k | k == 1 = []
                | otherwise = combineList n (k - 1)



{-caso base: combineList ? 1
usou 2 etapas com compreensao de listas
combineList n k | k == 1 = []
				| otherwise ... | .. , combineList n k-1

combineList 4 1 |
combineList n k | k == 1 = ......
                            | otherwise [...| , combineList n (k-1), ]

-}


