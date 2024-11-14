module Escalas where

import Data.List
import Notas
import Intervalos
import Acordes

--Generar escalas y acordes como: Acorde {Cifrado, [NotaTA]}

printInAmerican :: [Nota] -> String
printInAmerican xs = foldl (\str n -> str ++ (showNotaAmericano n)) "" xs

esc_cromatica :: [Nota]
esc_cromatica = todas \\ elim
    where
        xs = map (\x -> newNote x N 1) notas_basicas
        xs2 = map sostenido xs
        elim = [(xs2 !! 2) , (last xs2)]
        todas = concat (zipWith (\x y -> [x] ++ [y]) xs xs2)

escMayor :: Nota -> [Nota]
escMayor = aplicarIntervalos intervalos
    where
    intervalos = [I(xs) | xs <- tuplas]
    n = [2..7]
    modifs = [Maj, Maj, J, J, Maj, Maj, Maj]
    tuplas = zip n modifs

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

