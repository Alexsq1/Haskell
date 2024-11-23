module Testing where

import Test.QuickCheck
import Notas
import Intervalos
import Escalas
import Acordes



--Propiedades de notas

instance Arbitrary BPC where
    arbitrary = elements all_notas_basicas

instance Arbitrary Alteracion where
    arbitrary = elements all_alteraciones

instance Arbitrary Nota where
    arbitrary = do
        randomBPC <- arbitrary
        randomAlt <- arbitrary
        randomOct <- choose (0, 10)
        return $ newNote randomBPC randomAlt randomOct 

allModifs :: [Modif]
allModifs = [Dim .. ]

instance Arbitrary Modif where
    arbitrary = elements allModifs

instance Arbitrary Intervalo where
    arbitrary = do
        randomNum <- choose (0, 50)
        randomMod <- arbitrary
        return $ I(randomNum, randomMod)


(c:d:e:f:g:a:b:_) = generarAsc all_notas_basicas

p1 n = 
    (alt n /= BB) ==> 
    sostenido (bemol n) == n
    where t = (n :: Nota)

p2 xs = reverse (reverse xs) === xs
    where types = (xs :: [BPC])

p3 n1 n2 o1 o2 = 
    nf1 < nf2 ==>
--    n1 < n2 && (alt n1 == N && alt n2 == N) ==> 
    calcIntervalo nf1 (intervalo_2notas nf1 nf2) == nf2
    where 
        nf1 = newNote n1 N o1
        nf2 = newNote n2 N o2






{-
Ejemplos de quickcheck
p1 a b = a+b == b+a
p2 xs = 
    not (null xs) ==>
    (length $ tail xs) === ((length xs) -1)

p3 xs = collect (length xs) $ reverse (reverse xs) === xs
    where types = (xs :: [Int])


p4 a b = classify (a > 0 && b > 0) "positivos" 
    $ a+b == b+a
-}
