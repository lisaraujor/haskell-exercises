bigger :: Int -> [Int] -> Bool
bigger a x = (foldr (+) 0 x) >= a