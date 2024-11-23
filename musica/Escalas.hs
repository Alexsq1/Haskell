module Escalas where

import Data.List
import Util
import Notas
import Intervalos
import Acordes

--Generar escalas y acordes como: Acorde {Cifrado, [NotaTA]}

printInAmerican :: [Nota] -> String
printInAmerican xs = foldl (\str n -> str ++ (showNotaAmericano n)) "" xs

esc_cromatica :: [Nota]
esc_cromatica = todas \\ elim
    where
        xs = map (\x -> newNote x N 1) all_notas_basicas
        xs2 = map sostenido xs
        elim = [(xs2 !! 2) , (last xs2)]
        todas = concat (zipWith (\x y -> [x] ++ [y]) xs xs2)

--hacer escala general, con enteros y modifs
escHeptatonica :: [Modif] -> Nota -> [Nota]
escHeptatonica ints = aplicarIntervalos intervalos
    where
    intervalos = [I(xs) | xs <- tuplas]
    n = [2..8]
    tuplas = zip n (ints ++ [J])

escMayor :: Nota -> [Nota]
escMayor = escHeptatonica [Maj, Maj, J, J, Maj, Maj]

escDorica :: Nota -> [Nota]
escDorica = escHeptatonica [Maj, Min, J, J, Maj, Min]

escFrigia :: Nota -> [Nota]
escFrigia = escHeptatonica [Min, Min, J, J, Min, Min]

escLidia :: Nota -> [Nota]
escLidia = escHeptatonica [Maj, Maj, Aum, J, Maj, Maj]

escMixolidia :: Nota -> [Nota]
escMixolidia = escHeptatonica [Maj, Maj, J, J, Maj, Min]

escMenor :: Nota -> [Nota]
escMenor = escHeptatonica [Maj, Min, J, J, Min, Min]

escLocria :: Nota -> [Nota]
escLocria = escHeptatonica [Min, Min, J, Dim, Min, Min]

--Hacer menor armónica y melódica
--Hacer pentatónicas y blues

{-

Escalas:

Cromática

Mayor/Jónica
Menor natural
Menor armónica
Menor melódica

Dórica
Frigia
Lidia
Mixolidia
Locria

Pentatónica menor
Blues menor

Pentatónica mayor
Blues mayor


Otras

Bee bop
full step
Aumentada
Disminuida
Húngara
Árabe



-}

