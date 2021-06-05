digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

bigSum :: [Int] -> [Int]
bigSum lista = [digs z | z <- lista]