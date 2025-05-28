module Main (main) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Tipos de datos
type Matriz = [[Int]]
type Coordenada = (Int, Int)

-- Energía inicial del mago
energiaInicial :: Int
energiaInicial = 12

-- Matriz de runas
matrizRunas :: Matriz
matrizRunas = [[ 2, -3, 1, 0, 2, 3],
               [-5, 4, -2, 1, 0, -4],
               [ 1, 3, 0, -3, 2, 2],
               [ 2, -1, 4, 0, -5, 1],
               [ 0, 2, -3, 3, 4, -1],
               [ 1, 0, 2, -2, 1, 5]]

-- Función para obtener el valor de una celda
valorCelda :: Matriz -> Coordenada -> Int
valorCelda matriz (x, y) = (matriz !! x) !! y

-- Función para verificar si una coordenada está dentro de los límites de la matriz
esValida :: Matriz -> Coordenada -> Bool
esValida matriz (x, y) = x >= 0 && y >= 0 && x < length matriz && y < length (head matriz)

-- Función para calcular los movimientos posibles desde una coordenada
movimientosPosibles :: Matriz -> [Coordenada] -> Coordenada -> [(String, Coordenada)]
movimientosPosibles matriz visitadas (x, y) =
  let movimientos = [("Derecha", (x, y + 1)),
                     ("Abajo", (x + 1, y)),
                     ("Diagonal", (x + 1, y + 1)),
                     ("Izquierda", (x, y - 1)),
                     ("Arriba", (x - 1, y))]
  in filter (\(_, coord) -> esValida matriz coord && coord `notElem` visitadas) movimientos

-- Mostrar la matriz con la posición actual del mago
mostrarMatrizConPosicion :: Matriz -> Coordenada -> IO ()
mostrarMatrizConPosicion matriz (px, py) = do
  let matrizConPosicion = [[if (x, y) == (px, py) then "M" else show valor | (y, valor) <- zip [0..] fila] | (x, fila) <- zip [0..] matriz]
  putStrLn $ intercalate "\n" (map unwords matrizConPosicion)

-- Juego interactivo
jugar :: Matriz -> Coordenada -> Int -> [Coordenada] -> [String] -> IO ()
jugar matriz coord energia visitadas movimientosRealizados = do
  mostrarMatrizConPosicion matriz coord
  putStrLn $ "\nEnergía actual: " ++ show energia
  putStrLn $ "Posición actual: " ++ show coord
  if energia < 0
    then putStrLn "¡Energía agotada! Has perdido."
    else if coord == (length matriz - 1, length (head matriz) - 1)
      then do
        putStrLn $ "¡Has llegado al final con energía " ++ show energia ++ "!"
        putStrLn $ "Movimientos realizados: " ++ show movimientosRealizados
      else do
        let movimientos = movimientosPosibles matriz visitadas coord
        putStrLn "\nMovimientos posibles:"
        mapM_ (\(i, (desc, _)) -> putStrLn $ show i ++ ". " ++ desc) (zip [1..] movimientos)
        putStrLn "Elige un movimiento:"
        opcion <- getLine
        let eleccion = fromMaybe (-1) (readMaybe opcion)
        if eleccion < 1 || eleccion > length movimientos
          then do
            putStrLn "Opción inválida. Intenta de nuevo."
            jugar matriz coord energia visitadas movimientosRealizados
          else do
            let (desc, nuevaCoord) = movimientos !! (eleccion - 1)
            let valor = valorCelda matriz nuevaCoord
            let costo = if nuevaCoord == (fst coord + 1, snd coord + 1) then 2 else 0
            let penalizacion = if valor == 0 then 3 else 0
            let nuevaEnergia = energia + valor - costo - penalizacion
            jugar matriz nuevaCoord nuevaEnergia (nuevaCoord : visitadas) (movimientosRealizados ++ [desc])

-- Función para leer un número de forma segura
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-- Ejecución del programa
main :: IO ()
main = do
  putStrLn "Bienvenido al Bosque de las Runas Mágicas"
  putStrLn "Aquí está el mapa del bosque:"
  jugar matrizRunas (0, 0) energiaInicial [(0, 0)] []
