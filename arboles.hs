data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

t1 :: Tree Int
t1 = Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf) ) 1 (Node Leaf 3 (Node Leaf 8 Leaf))

memberTree :: Eq a => Tree a -> a -> Bool
memberTree Leaf _ = False
memberTree (Node t1 elem t2) a = (a == elem) || 
                                memberTree t1 a ||
                                memberTree t2 a

espejo :: Tree a -> Tree a
espejo Leaf = Leaf
espejo (Node t1 elem t2) = (Node (espejo t2) elem (espejo t1))

sumaTree :: Tree Int -> Int
sumaTree Leaf = 0
sumaTree (Node t1 elem t2) = elem + sumaTree t1 + sumaTree t2

preOrder :: Tree a -> [a]
preOrder Leaf = []
preOrder (Node t1 elem t2) = elem : (preOrder t1) ++ (preOrder t2) 

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node t1 elem t2) = (inOrder t1) ++ elem : (inOrder t2) 

posOrder :: Tree a -> [a]
posOrder Leaf = []
posOrder (Node t1 elem t2) = (posOrder t1) ++ (posOrder t2) ++ [elem]

hojas :: Tree a -> [a]
hojas Leaf = []
hojas (Node Leaf elem Leaf) = [elem]
hojas (Node t1 elem t2) = hojas t1 ++ hojas t2

internos :: Tree a -> [a]
internos Leaf = []
internos (Node Leaf elem Leaf) = []
internos (Node t1 elem t2) = elem : internos t1 ++ internos t2


