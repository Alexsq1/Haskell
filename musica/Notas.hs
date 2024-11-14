module Notas where

import Util

-- Lo más básico de las notas:

data BPC = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Enum, Eq, Ord)
data APC = C | D | E | F | G | A | B deriving (Show, Enum, Eq, Ord)

data Alteracion = BB | Bem | N | S | SS deriving (Eq, Enum, Ord)
data Cifrado = Tradicional | Americano deriving (Show, Eq)
--Sacar cifrado de notas?
type Octava = Int

data Nota = Nota {nota :: BPC, alt :: Alteracion, octava :: Octava} deriving (Eq)

newNote :: BPC -> Alteracion -> Octava -> Nota
newNote n alt oct = Nota {nota = n, alt = alt, octava = oct}

instance Show Nota where
    show (Nota nota alteracion octava) = show nota ++ show alteracion ++ show octava

showNotaAmericano :: Nota -> String
showNotaAmericano (Nota no alteracion octava) = show (tradicional_a_americano no) ++ show alteracion ++ show octava

instance Show Alteracion where
    show BB = "♭♭"
    show Bem = "♭"
    show N = " "
    show S = "♯"
    show SS = "𝄪"

instance Ord Nota where
    compare n1 n2
        | o1 /= o2 = compare o1 o2
        | bpc1 /= bpc2 = compare bpc1 bpc2
        | otherwise = compare (alt n1) (alt n2)
        where 
            o1 = octava n1
            o2 = octava n2
            bpc1 = nota n1
            bpc2 = nota n2

--    compare n1 n2 = compare (nota_to_PC n1) (nota_to_PC n2)

alt_to_semit :: Alteracion -> Int
alt_to_semit a = index alteraciones a - 2

nota_to_PC :: Nota -> Int
nota_to_PC (Nota n a e) = notabasica_to_pc n + alt_to_semit a + 12 * e
    where
        notabasica_to_pc nb
            | nb == Do = 0
            | nb == Re = 2
            | nb == Mi = 4
            | nb == Fa = 5
            | nb == Sol = 7
            | nb == La = 9
            | nb == Si = 11

notas_basicas :: [BPC]
notas_basicas = [Do .. ]

notas_americanas :: [APC]
notas_americanas = [C .. ]

alteraciones :: [Alteracion]
alteraciones = [BB .. ]


tradicional_a_americano :: BPC -> APC
tradicional_a_americano nota = notas_americanas !! i
    where i = index notas_basicas nota

alterar :: Nota -> Int -> Nota
alterar n x 
    | x < 0 = alterar (bemol n) (x+1)
    | x > 0 = alterar (sostenido n) (x-1)
    | otherwise = n


sostenido :: Nota -> Nota
sostenido (Nota n alteracion oct)
    | alteracion == SS = error "No acepto triples sostenidos"
    | otherwise = newNote n (ldesp alteracion alteraciones (+)) oct

bemol :: Nota -> Nota
bemol (Nota n alteracion oct)
    | alteracion == BB = error "No acepto triples bemoles"
    | otherwise = newNote n (ldesp alteracion alteraciones (-)) oct

dobleSostenido :: Nota -> Nota
dobleSostenido = (sostenido.sostenido)

dobleBemol :: Nota -> Nota
dobleBemol = (bemol.bemol)

ldesp :: (Enum a, Eq a) => a -> [a] -> (Int -> Int -> Int) -> a
ldesp x xs f = xs !! (f i 1)
    where i = index xs x
