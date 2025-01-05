module Search where


import Maze

adjacent :: Maze -> Square -> [Square]
adjacent m (x, y) = [(xa, ya) | xa <- [x - 1 .. x + 1], ya <- [y - 1 .. y + 1], ((x == xa) /= (y == ya)) && (not (isWall m (xa, ya)))  && validSquare m (xa, ya)]

euclidDistance :: Square -> Square -> Float
euclidDistance (x1, y1) (x2, y2) = sqrt (((xf1 - xf2) ^ 2) + ((yf1 - yf2) ^ 2))
    where
        [xf1, xf2, yf1, yf2] = map (fromInteger) [x1, x2, y1, y2]

manhattanDistance :: Square -> Square -> Integer
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

{-
Programar:
Búsqueda en anchura
Búsqueda en profundidad
IDDFS
A*
IDA*

-}