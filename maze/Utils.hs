module Utils where

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

--Recibe 2 listas ordenadas, las entrelaza
zipSort :: Ord a => [a] -> [a] -> [a]
zipSort [] xs = xs
zipSort xs [] = xs
zipSort (x:xs) (y:ys)
    | x < y = x : zipSort xs (y:ys)
    | otherwise = y : zipSort (x:xs) ys


unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter (/= x)) (unique xs)
