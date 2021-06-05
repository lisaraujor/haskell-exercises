{-oL :: [Int] -> [Int]
oL [] = []
oL (x:xs) = x ++ oL xs-}

{-g :: [Int] -> [Int]
g [] = []
g lista = cL (oL lista)-}


cL :: [Int] -> [Int]
cL [] = []
cL (x:xs) = (c x 1 xs) : cL (r x xs)


c :: Int -> Int -> [Int] -> (Int, Int)
c x a [] = (x,a)
c x a (y:ys) | x == y = c x (a+1) ys
             | otherwise = c x a ys


r :: Int -> [Int] -> [Int]
r _ [] = []
r x (y:ys) | x == y = r x ys
           | otherwise = y : r x ys


