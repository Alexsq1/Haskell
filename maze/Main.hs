module Main where

import Search
import Maze


main :: IO ()
main = do
    
--    let walls1 = ( [(1,y) | y <- [2 .. 8] ] ++ [(y,1) | y <- [1 .. 4] ] ++ [(y,1) | y <- [8 .. 9] ] ++ [(4,y) | y <- [4 .. 8] ] ++ [(6,y) | y <- [2 .. 8] ]  ++ [(7,9),(5,3), (4,0)])
    let walls1 = (genWalls (1,2) (1,8)) ++ (genWalls (1,1) (4,1)) ++ (genWalls (8,1) (9,1)) ++ (genWalls (4,4) (4,8)) ++ (genWalls (6,2) (6,8)) ++ [(7,9), (5,3), (4,0)]
    let ms1 = newMaze 10 10 walls1
    let mu1 = newMaze 10 10 ((4,2):walls1)
    let ini = (0,0)
    let fin = (9,9)
    
    allSearchs ms1 ini fin
    allSearchs mu1 ini fin

    --let walls2 = ([(x,y) | x <- [1,3 .. 18], y <- [1 .. 18]] ++ [(x,0) | x <- [1,5 .. 18]] ++ [(x,19) | x <- [3,7 .. 18]] )
    --let ms2 = newMaze 20 20 walls2
    --let mu2 = newMaze 20 20 ((18,19):walls2)
    --let fin = (19,19)
    --
    --allSearchs ms2 ini fin
    --allSearchs mu2 ini fin

    --let walls2 = ([(x,y) | x <- [1,3 .. 18], y <- [1 .. 18]] ++ [(x,0) | x <- [1,5 .. 18]] ++ [(x,19) | x <- [3,7 .. 18]] )
    let ms3 = newMaze 20 20 walls3
    let mu3 = newMaze 20 20 ((16,16):walls3)
    
    allSearchs ms3 ini fin
    allSearchs mu3 ini fin



allSearchs :: Maze -> Square -> Square -> IO()
allSearchs m ini fin = do

    putStrLn "Probando para el laberinto: "
    putStrLn $ show m

    putStrLn $ "\nProbando dfs:\n" ++ show (dfs m 1000 ini fin)
    putStrLn $ "\nProbando bfs:\n" ++ show (bfs m ini fin)
    putStrLn $ "\nProbando iddfs:\n" ++ show (iddfs m ini fin)
    putStrLn $ "\nProbando simpleIddfs:\n" ++ show (simpleIddfs m ini fin)
    putStrLn $ "\nProbando A* en laberinto:\n" ++ show (astar m (manhattanDistance fin) ini fin)
    putStrLn $ "\nProbando IDA* en laberinto:\n" ++ show (idaStar m (manhattanDistance fin) ini fin)
