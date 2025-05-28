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
```

- Energía inicial: `12`

#### Salida Esperada

- Lista con las coordenadas del camino válido con mayor energía final.  
- Energía final.

### 2.3 Restricciones de Implementación

- Usar programación funcional pura (sin variables mutables, ni bucles).  
- Solución basada en recursión y/o uso de `map`, `filter`, `reduce`.  
- Lenguaje obligatorio: **Haskell**

---

## 3. Bonus y Detalles de la Entrega

- Agregar runas especiales:
  - `"T"` (teletransportador): envía al mago a otra coordenada si tiene energía suficiente.  
  - `"D"` (doble salto): permite movimiento diagonal especial.

- **Fecha de entrega:** 9 y 10 de junio, 23:59 hrs (según sección).  
- Por cada día de atraso, se descuenta 1 punto desde las 00:00 del día siguiente.

---

## 4. Formato de Entrega (GitHub)

### Repositorio

- Crear un repositorio llamado `App3`.  
- Repositorio privado, incluir a todos los integrantes, profesores y ayudante como colaboradores.  

### Uso de Git

- Commits equilibrados entre todos los integrantes.  
- Se evaluará el historial de commits y uso de Pull Requests.

### Estructura del Repositorio

- Código fuente en Haskell, organizado y documentado.  
- Reflexiones finales / Autoevaluación:
  - ¿Qué fue lo más desafiante de implementar en paradigma funcional?
  - ¿Qué aprendizajes surgieron del proyecto?
- Explicación del uso de IA (si aplica):
  - ¿Qué tipo de ayuda proporcionó la herramienta?
  - ¿Cómo validaron o contrastaron las sugerencias?
- `README.md` con:
  - Instrucciones de compilación y ejecución.
  - Datos de los integrantes (nombre, correo, etc.).

---

## 5. Rúbrica de Evaluación

| Criterio | Peso | Descripción |
|---------|------|-------------|
| Funcionamiento general | 30% | Compila y ejecuta correctamente; calcula el mejor camino según las reglas. |
| Paradigma funcional | 30% | Uso de funciones puras, recursión, inmutabilidad, composición, etc. |
| Informe de diseño y reflexiones | 10% | Documento claro que explique diseño, decisiones y mejoras. |
| Git y organización | 20% | Commits equilibrados, estructura clara, `README.md` completo. |
| Presentación | 10% | Presentación funcional del proyecto en clases. |

---

## 6. Ejemplo de Uso

```bash
App3 MatrizBosqueInicial EnergiaInicial
```

---

## 7. Conclusión

La App #3 busca afianzar conocimientos del paradigma funcional usando Haskell. Se espera un uso adecuado de Git para evidenciar el trabajo colaborativo y el aprendizaje de cada integrante.

---
