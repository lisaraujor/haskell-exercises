somaTuplas :: (Int,Int) -> Int
somaTuplas (a,b) = (a + b)  


maiorTupla :: [(Int,Int)] -> (Int,Int)
maiorTupla [] = (0,0)
maiorTupla ((a, b):xs) | (somaTuplas (a,b)) > (somaTuplas (as,bs)) = (a,b)
                       | otherwise = (as,bs)
                       where (as,bs) = maiorTupla xs


menorTupla :: [(Int,Int)] -> (Int,Int)
menorTupla [] = (237,237)
menorTupla ((a, b):xs) | (somaTuplas (a,b)) < (somaTuplas (as,bs)) = (a,b)
                       | otherwise = (as,bs)
                       where (as,bs) = menorTupla xs



minMaxSum :: [(Int,Int)] -> ((Int,Int), (Int,Int))
minMaxSum [] = ((0,0), (0,0))
minMaxSum lista = ((menorTupla lista), (maiorTupla lista))



