{-aux :: Int -> [Int]
aux num = [0..num-1]

combineList :: Int -> Int -> [[Int]]
combineList comb qntd = [take qntd [0..comb-1] | x <- aux comb]-}

lista :: [Int] -> [Int] -> [[Int]]
lista lista1 lista2 = [take 3[x, lista2] | x <-lista1]

