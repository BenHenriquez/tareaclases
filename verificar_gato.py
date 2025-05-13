def leer_tablero(nombre_archivo):
    with open(nombre_archivo, 'r') as archivo:
        return [list(line.strip()) for line in archivo.readlines()]

def hay_ganador(tablero):
    lineas = []

    # Filas y columnas
    for i in range(3):
        lineas.append(tablero[i])  # filas
        lineas.append([tablero[0][i], tablero[1][i], tablero[2][i]])  # columnas

    # Diagonales
    lineas.append([tablero[0][0], tablero[1][1], tablero[2][2]])
    lineas.append([tablero[0][2], tablero[1][1], tablero[2][0]])

    for linea in lineas:
        if linea[0] != ' ' and linea.count(linea[0]) == 3:
            return True, linea[0]
    return False, None

if __name__ == "__main__":
    tablero = leer_tablero("tablero.txt")
    ganador, jugador = hay_ganador(tablero)
    if ganador:
        print(f"¡El jugador '{jugador}' ha ganado!")
    else:
        print("No hay ganador.")
