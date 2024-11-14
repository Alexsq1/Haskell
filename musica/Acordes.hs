module Acordes where 

import Notas
import Intervalos

doMayor :: [Nota]
doMayor = [n1, n2, n3]
    where 
        n1 = newNote Do N 1
        n2 = newNote Mi N 1
        n3 = newNote Sol N 1

generarIntervalos :: [Nota] -> [Intervalo]
generarIntervalos xs = map (intervalo_2notas head) tail
    where (head:tail) = xs

aplicarIntervalos :: [Intervalo] -> Nota -> [Nota]
aplicarIntervalos xs n
    | any (not.intervValido) xs = error "Intervalo no válido"
    | otherwise = n : map (\interv -> calc_intervalo n interv) xs




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
