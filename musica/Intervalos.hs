module Intervalos where

--Puede ser útil sólo exportar intervalo entre 2 notas y calcular.
--El resto son métodos auxiliares


import Notas
import Util


data Modif = Dim | Min | J | Maj | Aum  deriving (Show, Eq, Enum, Ord)
data Intervalo = I (Int, Modif) deriving (Eq)

instance Show Intervalo where
    show (I (int, modif)) = (show int) ++ "ª " ++ (show modif)

instance Ord Intervalo where
    compare (I(n1, m1)) (I(n2, m2))
        | n1 /= n2 = compare n1 n2
        | otherwise = compare m1 m2

instance Enum Intervalo where
    fromEnum (I(n, m)) = 5 * (fromEnum n) + fromEnum m
    toEnum x = I(n, toEnum (m))
        where
            (n, m) = int_division x 5

semitonosDif2Notas :: Nota -> Nota -> Int
semitonosDif2Notas n1 n2 = nota_to_PC n2 - nota_to_PC n1

intervValido :: Intervalo -> Bool
intervValido (I (num, modif))
        | num > 8 = intervValido (I(modul, modif))
        | otherwise = posibles `member` modif
    where
        posibles = modifsPosibles num
        modul = modOctava num

modOctava :: Int -> Int
modOctava n = 2 + (n-2) `mod` 7
--toma num, devuelve eq de octava menos

modifsPosibles :: Int -> [Modif]
modifsPosibles n
    | n > 8 = modifsPosibles (modOctava n)
    | min_maj `member` n = [Dim, Min, Maj, Aum]
    | justas `member` n = [Dim, J, Aum]
    | otherwise = []
    where
        min_maj = [2, 3, 6, 7]
        justas = [4, 5, 8]
        
intervalo_2notas :: Nota -> Nota -> Intervalo
intervalo_2notas n1 n2
    | n1 >= n2 = error "Intervalo igual o negativo"
    | otherwise = I(7*escalas + i + 1, mod)
    where 
        (Nota nb1 alt1 esc1) = n1
        (Nota nb2 alt2 esc2) = n2
        escalas = esc2 - esc1
        i = (index notas_basicas nb2) - (index notas_basicas nb1)
        iPuro = 7*escalas + i + 1
        mod = modif iPuro (semitonosDif2Notas n1 n2)

--Dado el intervalo y los semitonos, calcula el modificador
--2º, 2 semits -> mayor
modif :: Int -> Int -> Modif
modif intervalo semitonosDif2Notas
    | mp !! 1 == Min = mp !! (2+ajuste)     --Si es min, maj
    | otherwise = mp !! (1+ajuste)          --Si es justa
    where
        mp = modifsPosibles intervalo
        ajuste = semitonosDif2Notas - inter_semit_canonico (intervalo)

inter_semit_canonico :: Int -> Int    --Desde intervalo
inter_semit_canonico n
    | n > 8 = 12 + inter_semit_canonico (n-7)
    | otherwise = inter_semit_canonico_oct n
    where
        inter_semit_canonico_oct 2 = 2
        inter_semit_canonico_oct 3 = 4
        inter_semit_canonico_oct 4 = 5
        inter_semit_canonico_oct 5 = 7
        inter_semit_canonico_oct 6 = 9
        inter_semit_canonico_oct 7 = 11
        inter_semit_canonico_oct 8 = 12

--semits_de_intervalo :: Intervalo -> Int
--semits_de_intervalo I(n,m) = inter_semit_canonico n + 

semits_de_intervalo :: Intervalo -> Int
semits_de_intervalo (intervalo)
    | not (intervValido intervalo) = error "Intervalo no valido"
    | otherwise = semitonosDif2Notas n0 n1
    where
        n0 = newNote Do N 0
        n1 = calc_intervalo n0 intervalo

--falla
calc_intervalo :: Nota -> Intervalo -> Nota
calc_intervalo n (I(interv, mod))
    | not (intervValido (I(interv,mod))) = error "Intervalo no válido"
    | (!!) xs 1 == J = alterar (canonic_calc_intervalo n interv) (posx - 1)
    | otherwise = alterar (canonic_calc_intervalo n interv) (posx - 2)
    where
        xs = modifsPosibles interv
        posx = index xs mod

canonic_calc_intervalo :: Nota -> Int -> Nota
canonic_calc_intervalo (Nota n alt oct) interv 
    | otherwise = newNote n2 alt2 oct2
    where
        i1 = index notas_basicas n
        i2 = i1 + interv - 1
        (cambioOct, posx) = int_division i2 7
        n2 = notas_basicas !! posx
        oct2 = oct + cambioOct
        alt2 = N

inversion :: Intervalo -> Intervalo
inversion intervalo 
    | not (intervValido intervalo) = error "Intervalo no valido"
    | octavas_exactas num = intervalo_2notas n1 (newNote Do N (2 * difOctavas))
    | otherwise = intervalo_2notas n1 (newNote Do N (1 + 2 * difOctavas))
    where
        I(num, _) = intervalo
        n0 = newNote Do N 0
        n1 = calc_intervalo n0 intervalo
        difOctavas = (octava n1) - (octava n0)

octavas_exactas :: Int -> Bool
octavas_exactas x = (x-1) `mod` 7 == 0

transponer :: Intervalo -> Nota -> Nota
transponer i n = calc_intervalo n i
