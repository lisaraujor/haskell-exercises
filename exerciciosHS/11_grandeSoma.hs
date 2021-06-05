sumDig :: Int -> Int
sumDig 0 = 0
sumDig x | x >= 0 && x <= 9 = x
         | x >= 10 && x <= 99 = (mod(div x 10) 10) + (mod x 10)
         | x >= 100 && x <= 999 = (div x 100) + (mod(div x 10) 10) + (mod x 10)
         | otherwise = (div x 1000) + (div x 100) + (mod(div x 10) 10) + (mod x 10)


transf :: [Int] -> [Int]
transf [] = []
transf lista = [sumDig x | x <- lista]

bigSum :: [Int] -> Int
bigSum [x] = x
bigSum (x:xs) | ((sumDig x) >  bigSum (transf xs)) = x
              | otherwise = bigSum xs