comparar :: [Int] -> [Int] -> Bool
comparar [] [] = True
comparar [] _ = False
comparar _ [] =  False
comparar (a:b) (c:d) | (a == c) = comparar b d
                     | otherwise = False