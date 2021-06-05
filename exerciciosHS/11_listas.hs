get_maior :: [Int] -> [Int]
get_maior (x:xs) = [z | z <- xs, z > get_sec (x:xs)]

get_sec :: [Int] -> Int
get_sec lista = lista !! 1

teste :: [Int] -> [Int]
teste [] = []
teste lista = [lista !! 0, get_sec lista] ++ get_maior lista
