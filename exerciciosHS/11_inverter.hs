split :: String -> Char -> String
split [] _ = []
split (x:xs) c | c == x = xs
               | otherwise = split xs c

revert :: String -> String
revert [] = []
revert (x:xs) = (revert xs) ++ [x]

-----------------------------------------------------------

sW :: String -> Char -> String -> [String]
sW "" n "" = [""]
sW "" n b = [b]
sW (a:as) n b | n == a = b : sW as n ""
              | otherwise = sW as n (b++[a])


somarList :: [String] -> Char -> String
somarList [] _ = []
somarList list n = (reverse (head list)) ++ [n] ++ (somarList (tail list) n) invert str n = reverse (drop 1 (reverse (somarList (sW str n "") n)))
———————————-
split :: String -> [String]
split [] = [""]
split (x:xs) | x == ' ' = "" : (split cs)
             | otherwise = (c : head (split cs)) : tail (split cs)

revert :: [String] -> [String]
revert [] = []
revert (a:as) = (reverse a) : revert as
join :: Char -> [String] -> String
join _ [] = []
join _ (a:[]) = a
join c (a:as) = a ++ [c] ++ join c as
invert :: String -> Char -> String
invert [] _ = []
invert as c = join c (revert (split as c))