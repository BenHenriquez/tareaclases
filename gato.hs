import Data.List
import Data.Char
import System.Random -- Agrega esta línea

type Board = [Char] -- lista de 9 elementos

-- Muestra el tablero
printBoard :: Board -> IO ()
printBoard b = do
  putStrLn $ intercalate "\n-+-+-\n" $ map (intersperse '|') (chunksOf 3 b)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Comprueba si un jugador ganó
winner :: Board -> Char -> Bool
winner b p = any lineWin winningLines
  where
    lineWin [x,y,z] = b !! x == p && b !! y == p && b !! z == p
    winningLines = 
      [[0,1,2],[3,4,5],[6,7,8], -- filas
       [0,3,6],[1,4,7],[2,5,8], -- columnas
       [0,4,8],[2,4,6]]         -- diagonales

-- Comprueba si el tablero está lleno (empate)
fullBoard :: Board -> Bool
fullBoard = all (/= ' ')

-- Movimiento humano: pide y valida (versión corregida)
humanMove :: Board -> IO Board
humanMove b = do
  putStrLn "Tu turno! Ingresa posición (1-9):"
  inp <- getLine
  case inp of
    [c] | isDigit c -> doMove (digitToInt c)
    _ -> do
      putStrLn "Entrada inválida."
      humanMove b
  where
    doMove n
      | n < 1 || n > 9 = putStrLn "Posición inválida." >> humanMove b
      | b !! (n-1) /= ' ' = putStrLn "Posición ocupada." >> humanMove b
      | otherwise = return (take (n-1) b ++ ['X'] ++ drop n b)

-- Movimiento computadora: elige una casilla vacía al azar
computerMove :: Board -> IO Board
computerMove b = do
  let emptyIndices = [i | (i, c) <- zip [0..] b, c == ' ']
  if null emptyIndices
    then return b
    else do
      idx <- randomRIO (0, length emptyIndices - 1)
      let i = emptyIndices !! idx
      return (take i b ++ ['O'] ++ drop (i+1) b)

-- Juego recursivo
gameLoop :: Board -> IO ()
gameLoop b = do
  printBoard b
  if winner b 'O' then putStrLn "Computadora gana!"
  else if winner b 'X' then putStrLn "¡Tú ganas!"
  else if fullBoard b then putStrLn "Empate."
  else do
    b1 <- humanMove b
    if winner b1 'X' then do
      printBoard b1
      putStrLn "¡Tú ganas!"
    else if fullBoard b1 then do
      printBoard b1
      putStrLn "Empate."
    else do
      b2 <- computerMove b1
      gameLoop b2

main :: IO ()
main = do
  putStrLn "Bienvenido al gato 3x3"
  gameLoop (replicate 9 ' ')

