data Tree a = Nilt | Node a (Tree a) (Tree a) deriving (Show,Eq)

treeList :: Tree a -> [a]
treeList Nilt = []
treeList (Node no esq dir) = treeList esq ++ [no] ++ treeList dir

depth:: Tree a -> Int
depth Nilt = 0
depth (Node _ Nilt Nilt) = 0
depth (Node _ esq dir) = 1 + max' (depth esq) (depth dir)

max' :: (Eq a, Ord a, Show a) => a -> a -> a
max' a1 a2 | a1 >= a2 = a1
           | otherwise = a2