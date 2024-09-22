data Tupla = T Int Int

instance Show Tupla where
    show (T x y) 
        | y == 1 = show x
        | otherwise = let (T x2 y2) = simp (T x y)
            --in show (x2, y2)
            in show x2 ++ "/" ++ show y2

instance Eq Tupla where
    (T a b) == (T c d) = (a * d) == (c * b)

instance Ord Tupla where
    compare t1 t2 = comparar_fracs_simps (simp t1) (simp t2)
    

instance Num Tupla where
    (T a b) + (T c d) = simp $ T (a * d + c * b) (b * d)
    (T a b) - (T c d) = simp $ T (a * d - c * b) (b * d)
    (T a b) * (T c d) = simp $ T (a * c) (b * d)
    abs (T a b) = simp $ T (abs a) (abs b)
    signum (T a b) = T (signum a * signum b) 1
    fromInteger n = T (fromInteger n) 1
    negate (T a b) = simp (T ((-1) * a) b)
  
--Métodos míos

value :: Fractional a => Tupla -> a
value (T a b) = (fromIntegral a/ fromIntegral b)

inv :: Tupla -> Tupla
inv (T a b) = simp (T b a)

(//) :: Tupla -> Tupla -> Tupla
(//) t1 t2 = simp $ t1 * (inv t2)

expf :: Tupla -> Int -> Tupla
expf (T a b) n = T (a ^ n) (b ^ n)

--Métodos AUXILIARES

mcd :: Int -> Int -> Int
mcd x 0 = x
mcd x y = mcd y (x `mod` y)

simp :: Tupla -> Tupla
simp (T a b) = T (a `div` d) (b `div` d) 
    where d = mcd a b

comparar_fracs_simps :: Tupla -> Tupla -> Ordering
comparar_fracs_simps (T a b) (T c d) = compare (a*d) (c*b)


    