import Data.List

data Matriz = Matriz {nums :: [[Int]], filas :: Int, columnas :: Int}

instance Show Matriz where
    show (Matriz {nums, filas, columnas}) = toString nums ++ show (filas, columnas)
        where
            toString [] = ""
            toString (x:xs) = (show x) ++ ('\n' : toString xs)

instance Eq Matriz where
    x == y = 
        (filas x == filas y) && (columnas x == columnas y) && (nums x == nums y)


instance Num Matriz where
    x + y
        | eqDimensions x y == False = error "Matrices con distintas dimensiones"
        | otherwise = nuevaMatriz $ opElem (+) (nums x) (nums y)

    x - y
        | eqDimensions x y == False = error "Matrices con distintas dimensiones"
        | otherwise = nuevaMatriz $ opElem (-) (nums x) (nums y)

    x * y
        | columnas x /= filas y = error "Error de dimensiones multiplicando matrices"
        | otherwise = nuevaMatriz [prodFila x y i | i <- [0..filas x -1]]
        where prodFila x y i = [prodIJ x y i j | j <- [0..columnas y - 1]]

    abs x = nuevaMatriz ((map.map) (abs) (nums x))

    --signum x = signum (ijElem x 0 0)

    fromInteger x = nuevaMatriz [[fromIntegral x]]

    negate x = nuevaMatriz( (map.map) (* (-1)) (nums x))


--Más operaciones

(.*) :: Matriz -> Matriz -> Matriz
(.*) x y
    | eqDimensions x y == False = error "Matrices con distintas dimensiones"
    | otherwise = nuevaMatriz $ opElem (*) (nums x) (nums y)

esc_por_matriz :: Int -> Matriz -> Matriz
esc_por_matriz n m = nuevaMatriz ((map.map) (*n) (nums m))

potencia_naive :: Matriz -> Int -> Matriz
potencia_naive m n = paux m m n
    where paux acc m n 
                | n == 1 = acc
                | otherwise = paux (acc*m) m (n-1)

potencia :: Matriz -> Int -> Matriz
potencia m 1 = m
potencia m n
    | n `mod` 2 == 0 = (potencia m mid) * (potencia m mid)
    | otherwise = (potencia m mid) * (potencia m mid) * m
    where mid = n `div` 2



--CREACION

nuevaMatriz :: [[Int]] -> Matriz
nuevaMatriz xs = 
    if (matrizValida xs == False) then error "Matriz no válida" else
    let f = length xs
        c = length (xs !! 0)
    in Matriz {nums = xs, filas = f, columnas = c}

matrizValida :: [[Int]] -> Bool
matrizValida xs = 
  let long0 = length (xs !! 0) 
    in and ( map (\xs -> length xs == long0) xs)

ceros :: Int -> Int -> Matriz
ceros f c = nuevaMatriz(replicate f (replicate c 0))

identidad :: Int -> Matriz
identidad n = nuevaMatriz [filaCanonica n x | x <- [0..n-1]]

trianSup :: Int -> Matriz
trianSup m = nuevaMatriz (tsaux m (0))
    where 
        tsaux m n
            | n == m = []
            | otherwise = (replicate (n+1) 0 ++ replicate (m-n-1) 1) : tsaux m (n+1)

trianInf :: Int -> Matriz
trianInf m = transponer (trianSup m)

escalar :: Int -> Int -> Matriz
escalar tam n = esc_por_matriz n (identidad tam)

--AUXILIARES GENÉRICOS

mcd :: Int -> Int -> Int
mcd x 0 = abs(x)
mcd x y = mcd y (x `mod` y)

mcd_list :: [Int] -> Int
mcd_list [] = error "Lista vacía"
mcd_list (x:xs) = foldl mcd x xs


--AUXILIARES MATRICES

opElem :: (Int -> Int -> Int) -> [[Int]] -> [[Int]] -> [[Int]]
opElem f x y = zipWith (zipWith f) x y

iRow :: Matriz -> Int -> [Int]
iRow x n = nums x !! n

jCol :: Matriz -> Int -> [Int]
jCol x n = map (!! n) (nums x)

ijElem :: Matriz -> Int -> Int -> Int
ijElem x n m = (iRow x n) !! m

prodIJ :: Matriz -> Matriz -> Int -> Int -> Int
prodIJ m1 m2 i j = conv (iRow m1 i) (jCol m2 j)

conv :: [Int] -> [Int] -> Int
conv x y = sum (zipWith (*) x y)

flatRows :: Matriz -> [Int]
flatRows m = foldl (++) [] (nums m)

flatCols :: Matriz -> [Int]
flatCols m = foldl (++) [] (nums (transponer m))

transponer :: Matriz -> Matriz
transponer m = nuevaMatriz [jCol m x | x <- [0..columnas m -1]]

diagonal :: Matriz -> [Int]
diagonal m = elemsOverDiag m (==)

traza :: Matriz -> Int
traza m = sum (diagonal m)

prodDiag :: Matriz -> Int
prodDiag m = product (diagonal m)

elemsOverDiag :: Matriz -> (Int -> Int -> Bool) -> [Int]
elemsOverDiag m f
    | cuadrada m == False = error "Matriz no cuadrada"
    | otherwise = [ijElem m x y | x <- [0..filas m -1], y <- [0..columnas m -1], f x y]

filaCanonica :: Int -> Int -> [Int]
filaCanonica long posic = replicate posic 0 ++ [1] ++ replicate (long - posic - 1) 0

--Booleanos

eqDimensions :: Matriz -> Matriz -> Bool
eqDimensions x y = (filas x == filas y) && (columnas x == columnas y)

cuadrada :: Matriz -> Bool
cuadrada m = filas m == columnas m

simetrica :: Matriz -> Bool
simetrica m = m == transponer m

esMatrizDiagonal :: Matriz -> Bool
esMatrizDiagonal m = all (== 0) (elemsOverDiag m (/=))

esTrianSup :: Matriz -> Bool
esTrianSup m = all (==0) (elemsOverDiag m (>))

esTrianInf :: Matriz -> Bool
esTrianInf m = all (==0) (elemsOverDiag m (<))

esNula :: Matriz -> Bool
esNula m = (all.all) (==0) (nums m)

--(diagonal con elems iguales)
esEscalar :: Matriz -> Bool
esEscalar m = esMatrizDiagonal m && all (== head l) l
    where l = diagonal m



--GAUSS-JORDAN


--Pequeña api:
{-
Métodos: eij -> intercambio 2 filas
eik -> mult fila i por k
eijk -> fila i := fila i pre + k*fila j

-}

reemplazo :: Matriz -> Int -> [Int] -> Matriz
reemplazo m i fnueva
    | i >= (filas m) || length fnueva /= (columnas m) = error "Fallo de dimensiones al reemplazar"
    | otherwise = nuevaMatriz (take (i) (nums m) ++ [fnueva] ++ drop (i+1) (nums m))

swap :: Matriz -> Int -> Int -> Matriz
swap m i j
    | i >= (filas m) || j >= (filas m) = error "Fallo de filas en swap"
    | i == j = m
    | i > j = swap m j i
    | otherwise = nuevaMatriz (take (i) mat ++ [mat !! j] ++ [mat !! x | x <- [0..length mat -1], x > i, x < j] ++ [mat !! i] ++ drop (j+1) mat)
    where mat = nums m

map_fila :: Matriz -> Int -> Int -> Matriz
map_fila m i factor
    | i > (filas m) = error "Fallo de filas en swap"
    | otherwise = reemplazo m i (map (*factor) (nums m !! i) )

eijk :: Matriz -> Int -> Int -> Int -> Matriz
eijk m fMod fAux factor
    | fMod >= (filas m) || fAux >= (filas m) = error "Fallo de filas en eijk"
    | otherwise = reemplazo m fMod (zipWith (\x y -> x + y * factor) (iRow m fMod) (iRow m fAux))


simp_fila :: Matriz -> Int -> Matriz
simp_fila m i
    | all (==0) fila = m
    | otherwise = 
        let
        fila_nueva = map (\x -> x `div` (signo * mc)) fila
        in reemplazo m i fila_nueva
    where 
    fila = iRow m i
    mc = mcd_list fila
    signo = signum ( head (filter (/= 0) fila))


gauss :: Matriz -> Matriz
gauss m = pivote m (0,0)

pivote :: Matriz -> (Int, Int) -> Matriz
pivote m (i,j)
    | i >= (filas m) || j >= (columnas m) = m
    | col_vacia m (i,j) = pivote ms (i, j+1)
    | col_vacia_sin_pivote m (i, j) = pivote ms (i+1, j+1)
  --  | ijElem m i j == 0 = pivote m2 (i, j)
    | otherwise = pivote m2 (i,j)
    where
    ms = simp_fila m i
    m2 = hacer_ceros (bajar_ceros ms (i,j)) (i,j)





--Devuelve la columna del pivote, con el pivote INCLUIDO
col_pivote :: Matriz -> (Int, Int) -> [Int]
col_pivote m (i,j) = l \\ (take (i) l)
    where l = [x | x <- jCol m j ]

col_vacia :: Matriz -> (Int, Int) -> Bool
col_vacia m t = all (==0) (col_pivote m t)

col_vacia_sin_pivote :: Matriz -> (Int, Int) -> Bool
col_vacia_sin_pivote m t = all (==0) (tail(col_pivote m t))

bajar_ceros :: Matriz -> (Int, Int) -> Matriz
bajar_ceros m (i,j)
    | ijElem m i j /= 0 = m
    | otherwise = bajar_ceros2 m (i,j) (i+1,j)

bajar_ceros2 m (i,j) (i2, j2)
    | ijElem m i2 j == 0 = bajar_ceros2 m (i,j) (i2+1, j)
    | otherwise = swap m i i2


hacer_ceros :: Matriz -> (Int, Int) -> Matriz
hacer_ceros m (i,j) = hacer_ceros2 m (i,j) (i+1, j)


hacer_ceros2 m (i,j) (i2, j2)
    | i2 >= (filas m) = m
    | ijElem m i2 j2 == 0 = hacer_ceros2 m (i,j) (i2+1, j)
    | otherwise = hacer_ceros2 m4 (i,j) (i2+1, j)
    where 
        pivote = ijElem m i j
        act = ijElem m i2 j
        mcdivis = mcd pivote act
    --    mcmult = pivote * act `div` mcdivis
        m2 = map_fila m i2 (pivote `div` mcdivis)
        m3 = eijk m2 i2 i ((-1) * ijElem m2 i2 j `div` pivote)
        m4 = simp_fila m3 i

rango :: Matriz -> Int
rango m = length (filter ( any(/=0)) (nums ms))
    where 
    ms = gauss m
    

--revisar metodos nums

--gauss             HECHOOOOOOOOOOOOOOOOO
--gauss-jordan
--rango
--resolver sistemas
--determinante
--inversa
