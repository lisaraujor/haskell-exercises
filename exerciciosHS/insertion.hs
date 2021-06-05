import System.IO

print ("oi")

leitura :: IO ()
leitura = do
	      arq <- openFile "1000.txt" ReadMode
	      conteudo <- hGetContents arq
	      putStrLn conteudo
	      hClose arq


insertionSort :: IO() -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
                     where (x:xs) = leitura



insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y : (insert x ys)