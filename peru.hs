
import Data.List (nub)

type Coord = (Int, Int)
type Camino = [Coord]
type Laberinto = [[Int]]

main :: IO ()
main = do
    contenido <- readFile "input.txt"
    let laberinto = map (map read . words) $ filter (not . null) $ lines $ filter (/= '\r') contenido
    putStrLn "Laberinto:"
    mapM_ (putStrLn . unwords . map show) laberinto
    let soluciones = encontrarCaminos laberinto
    putStrLn "\nSoluciones encontradas:"
    putStrLn (mostrarSoluciones soluciones)
    writeFile "output.txt" (mostrarSoluciones soluciones)

encontrarCaminos :: Laberinto -> [Camino]
encontrarCaminos lab = 
    let filas = length lab
        cols = if filas > 0 then length (head lab) else 0
        entradas = [(i, 0) | i <- [0..filas-1], i < filas, lab !! i !! 0 == 0]
        salidas = [(i, cols-1) | i <- [0..filas-1], i < filas, lab !! i !! (cols-1) == 0]
    in if null entradas || null salidas
       then []
       else buscarCaminos lab (head entradas) (head salidas) [[head entradas]]

buscarCaminos :: Laberinto -> Coord -> Coord -> [Camino] -> [Camino]
buscarCaminos lab inicio fin caminosActuales =
    let caminosCompletos = filter ((== fin) . head) caminosActuales
        nuevosValidos = concatMap (expandirCamino lab fin) $ 
                       filter ((/= fin) . head) caminosActuales
    in if null nuevosValidos
       then caminosCompletos
       else nub $ caminosCompletos ++ buscarCaminos lab inicio fin nuevosValidos

expandirCamino :: Laberinto -> Coord -> Camino -> [Camino]
expandirCamino lab fin camino@(pos:_) = 
    if pos == fin 
    then [camino]
    else [pos':camino | pos' <- movimientosPosibles lab pos,
                       not (pos' `elem` camino)]
expandirCamino _ _ [] = []

movimientosPosibles :: Laberinto -> Coord -> [Coord]
movimientosPosibles lab (i, j) =
    [(i', j') | (i', j') <- [(i+1,j), (i-1,j), (i,j+1), (i,j-1)],
                i' >= 0, i' < length lab,
                j' >= 0, j' < length (head lab),
                lab !! i' !! j' == 0]

mostrarSoluciones :: [Camino] -> String
mostrarSoluciones [] = "No se encontraron soluciones"
mostrarSoluciones caminos = unlines $ map mostrarCamino caminos

mostrarCamino :: Camino -> String
mostrarCamino camino = "Camino: " ++ show (reverse camino)
