# tareaclases

# App 3 – El Bosque de las Runas Mágicas (Paradigma Funcional)

## TICS200 - LPP 2024

### Profesores
- María Loreto Arriagada – [loreto.arriagada.v@edu.uai.cl](mailto:loreto.arriagada.v@edu.uai.cl)  
- Paulina González – [paulina.gonzalez.p@edu.uai.cl](mailto:paulina.gonzalez.p@edu.uai.cl)  
- Justo Vargas – [justo.vargas@edu.uai.cl](mailto:justo.vargas@edu.uai.cl)  

### Ayudante
- Diego Duhalde – [dduhalde@alumnos.uai.cl](mailto:dduhalde@alumnos.uai.cl)

---

## 1. Objetivos

- Comprender el paradigma de Programación Funcional.
- Practicar herramientas de apoyo como Git (fork, pull requests, commits balanceados), debuggers, etc.

---

## 2. Enunciado

Un mago quiere atravesar un bosque encantado lleno de runas que modifican su energía. Cada celda del bosque contiene una runa con un valor (positivo o negativo). El mago parte desde la esquina superior izquierda y quiere llegar a la esquina inferior derecha maximizando su energía restante.

### 2.1 Requerimientos Funcionales

1. El bosque es una matriz de enteros (puede ser de tamaño NxN).  
2. El mago puede moverse:
   - A la derecha o hacia abajo.  
   - En sentido diagonal abajo-derecha.  
   - A la izquierda o hacia arriba (solo si no vuelve a una celda ya visitada).  
3. Los movimientos diagonales consumen 2 unidades extra de energía.  
4. Si se pasa por una celda con valor 0, es una trampa: pierde 3 puntos de energía adicionales.  
5. En cada celda, se suma (o resta) el valor de la runa a su energía.  
6. El mago comienza con una energía inicial (por ejemplo, 12).  
7. El recorrido final debe ser uno de los caminos posibles que deje al mago con la mayor energía posible al final.  
8. Si en algún momento la energía es menor que 0, el camino se invalida.  

### 2.2 Supuestos y Detalles

#### Entrada

- Matriz de runas:
```haskell
[[ 2, -3, 1, 0, 2, 3],
 [-5, 4, -2, 1, 0, -4],
 [ 1, 3, 0, -3, 2, 2],
 [ 2, -1, 4, 0, -5, 1],
 [ 0, 2, -3, 3, 4, -1],
 [ 1, 0, 2, -2, 1, 5]]
