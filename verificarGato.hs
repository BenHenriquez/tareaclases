import System.IO

type Tablero = [[Char]]

main :: IO ()
main = do
    contenido <- readFile "tablero.txt"
    let tablero = lines contenido  -- NO eliminamos espacios, cada línea debe tener exactamente 3 caracteres
    print tablero  -- opcional: para ver cómo se leyó el tablero
    case ganador tablero of
        Just jugador -> putStrLn $ "¡El jugador '" ++ [jugador] ++ "' ha ganado!"
        Nothing      -> putStrLn "No hay ganador."

ganador :: Tablero -> Maybe Char
ganador t = case filter lineaGanadora (lineas t) of
    ((x:_):_) -> Just x
    _         -> Nothing

lineaGanadora :: [Char] -> Bool
lineaGanadora [a, b, c] = a /= ' ' && a == b && b == c
lineaGanadora _         = False

lineas :: Tablero -> [[Char]]
lineas t =
    let filas = t
        columnas = [[t !! 0 !! i, t !! 1 !! i, t !! 2 !! i] | i <- [0..2]]
        diagonales = [[t !! 0 !! 0, t !! 1 !! 1, t !! 2 !! 2],
                      [t !! 0 !! 2, t !! 1 !! 1, t !! 2 !! 0]]
    in filas ++ columnas ++ diagonales
