-- `
prueba = print("Hola programador")
prueba2 = "hola mundo"
minimo = min 3 2 
suma a b = a+b
parImpar x = if x `mod` 2 == 0 then "par" else "impar"

main = do
  putStrLn "¿Cuál es tu nombre?"
  nombre <- getLine
  putStrLn ("Hola, " ++ nombre ++ "!")

main2 = do
  putStrLn "¿Cómo te llamas?"
  nombre <- getLine

  putStrLn "¿Cuántos años tienes?"
  edadStr <- getLine
  let edad = read edadStr :: Int

  putStrLn ("Hola, " ++ nombre ++ ". Tienes " ++ show edad ++ " años.")
