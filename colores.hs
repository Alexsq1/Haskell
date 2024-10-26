type Point = (Coord, Coord, Coord)
data Color = Rojo | Azul | VerdeClaro | VerdeOscuro | Amarillo | Morado | Rosa | Marron | Celeste | Naranja | Blanco | Negro | Gris deriving (Show, Eq, Enum, Bounded)
data Coord = C Int deriving (Eq, Ord)
data DistP = DP(Color, Int) deriving (Eq, Show)

instance Bounded Coord where
    minBound = C 0
    maxBound = C 255
instance Show Coord where
    show (C x) = show x
instance Ord DistP where
    compare (DP t1) (DP t2) = compare (snd t1) (snd t2)

newPoint :: Int -> Int -> Int -> Point
newPoint x y z = (newCoord x, newCoord y, newCoord z)

newCoord :: Int -> Coord
newCoord x
    | valid x = C x
    | otherwise = error "Las coordenadas tienen que estar entre 0 y 255"

valid :: Int -> Bool
valid x = x >= 0 && x <= 255

distance :: Point -> Point -> Int
distance (C x1, C y1, C z1) (C x2, C y2, C z2) = (x1 - x2) ^ 2 + (y2 - y1) ^ 2 + (z1 - z2) ^ 2

coord :: Color -> Point

coord Rojo = newPoint 255 0 0
coord VerdeClaro = newPoint 0 255 0
coord Azul = newPoint 0 0 255

coord Amarillo = newPoint 255 255 0
coord Morado = newPoint 177 0 255
coord Celeste = newPoint 0 255 255

coord VerdeOscuro = newPoint 0 177 0

coord Naranja = newPoint 255 177 0

coord Blanco = newPoint 255 255 255
coord Negro = newPoint 0 0 0
coord Gris = newPoint 177 177 177

coord Rosa = newPoint 255 63 191
coord Marron = newPoint 100 50 40


allColors :: [Color]
allColors = [Rojo .. ]

lista_coords :: [(Color, Point)]
lista_coords = zip allColors (map coord allColors)

lista_dist :: Point -> [DistP]
lista_dist orig = map (\(color, punto) -> DP(color, distance orig punto) ) lista_coords

map_fst :: (a -> c) -> [(a, b)] -> [(c, b)]
map_fst f = map (\(x, y) -> (f x, y))

map_snd :: (b -> c) -> [(a, b)] -> [(a, c)]
map_snd f = map (\(x, y) -> (x, f y))

unwrap :: DistP -> (Color, Int)
unwrap (DP t) = t

clasificador :: Point -> Color
clasificador p = fst (unwrap (minimum (lista_dist p)))

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups(filter (/=x) xs)

todos :: [Color]
todos = [clasificador (newPoint x y z) | x<-l, y<-l, z<-l]
    where l = 0:[5,10..255]

porcentajes :: [(Color, Int)]  
porcentajes = [(x, count x todos) | x <- allColors]

--Fallos raros (52, 179, 166), (62, 8, 133)