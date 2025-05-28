module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

type Matriz = [[Int]]
type Coordenada = (Int, Int)
type Camino = [Coordenada]

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
movimientosPosibles :: Matriz -> Camino -> Coordenada -> [Coordenada]
movimientosPosibles matriz visitadas (x, y) =
  filter (\coord -> esValida matriz coord && coord `notElem` visitadas)
    [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x - 1, y), (x, y - 1)]

-- Función recursiva para explorar todos los caminos posibles
explorarCaminos :: Matriz -> Camino -> Int -> Coordenada -> [(Camino, Int)]
explorarCaminos matriz visitadas energiaActual coord
  | energiaActual < 0 = [] -- Camino inválido si la energía es menor que 0
  | coord == (length matriz - 1, length (head matriz) - 1) = [(reverse (coord : visitadas), energiaActual)] -- Camino completo
  | otherwise = concatMap explorarSiguiente (movimientosPosibles matriz (coord : visitadas) coord)
  where
    valor = valorCelda matriz coord
    energiaNueva
      | valor == 0 = energiaActual - 3 -- Penalización por celda trampa
      | otherwise = energiaActual + valor
    explorarSiguiente siguienteCoord =
      let costoMovimiento = if siguienteCoord == (fst coord + 1, snd coord + 1) then 2 else 0
      in explorarCaminos matriz (coord : visitadas) (energiaNueva - costoMovimiento) siguienteCoord

-- Función principal para encontrar el mejor camino
mejorCamino :: Matriz -> Int -> (Camino, Int)
mejorCamino matriz energiaInicial =
  maximumBy (comparing snd) (explorarCaminos matriz [] energiaInicial (0, 0))

-- Ejecución del programa
main :: IO ()
main = do
        let (camino, energiaFinal) = mejorCamino matrizRunas energiaInicial
        putStrLn $ "Mejor camino: " ++ show camino
        putStrLn $ "Energía final: " ++ show energiaFinal
