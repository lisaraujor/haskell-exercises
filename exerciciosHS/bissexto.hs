leap :: Int -> Char
leap ano | ano `mod` 4 == 0 = 'Y'
         | otherwise = 'N'