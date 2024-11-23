module Acordes where 

import Notas
import Intervalos

acorde :: [Int] -> [Modif] -> Nota -> [Nota]
acorde ints mods = aplicarIntervalos intervalos
    where
        intervalos = [I(xs) | xs <- tuplas]
        tuplas = zip ints mods

acordeTriada :: [Modif] -> Nota -> [Nota]
acordeTriada = acorde [3,5]

trMayor :: Nota -> [Nota]
trMayor = acordeTriada [Maj, J]

trMenor :: Nota -> [Nota]
trMenor = acordeTriada [Min, J]

trAumentada :: Nota -> [Nota]
trAumentada = acordeTriada [Maj, Aum]

trDisminuida :: Nota -> [Nota]
trDisminuida = acordeTriada [Min, Dim]

generarIntervalos :: [Nota] -> [Intervalo]
generarIntervalos xs = map (intervalo_2notas head) tail
    where (head:tail) = xs

aplicarIntervalos :: [Intervalo] -> Nota -> [Nota]
aplicarIntervalos xs n
    | any (not.intervValido) xs = error "Intervalo no válido"
    | otherwise = n : map (\interv -> calcIntervalo n interv) xs




--Puede ser útil:

{-
Qué hacer ahora

Testing por propiedades, quickcheck

Probar y revisar todo
Ver cómo hacer el método read, para dar valores más sencillo
Ver cómo generar acordes y escalas

Siguiente paso: 
Calcular modos tonales con acordes.
Ejemplo: Mim ReM DoM    -> I VI VII

Notas/acordes y que te de la tonalidad

-}





{-

Acordes

(2)
quinta
power chord

(3)

menor
mayor
aumentado
disminuido
fácil

sus2
sus4

(4)

7
maj 7
min 7
semidisminuido
min3maj7

(acordes add -> add 9)

novena
(muchos de extensiones: 6, 9, 11, 13)

()
-}
