gauss_jordan <- function(A, b) {
  n <- nrow(A)
  
  # Construir la matriz aumentada [A|b]
  Augmented <- cbind(A, b)
  
  # Método de Gauss-Jordan
  for (i in 1:n) {
    # Hacer que el pivote sea 1
    Augmented[i,] <- Augmented[i,] / Augmented[i,i]
    
    # Eliminar las otras entradas de la columna
    for (j in 1:n) {
      if (i != j) {
        Augmented[j,] <- Augmented[j,] - Augmented[j,i] * Augmented[i,]
      }
    }
  }
  
  # El resultado es la última columna de la matriz aumentada
  return(Augmented[, n+1])
}

menu <- function() {
  cat("\nPrograma para resolver un sistema de ecuaciones de n variables usando el método de Gauss-Jordan\n")
  
  n <- as.integer(readline(prompt = "Ingrese el número de variables (n): "))
  
  # Crear la matriz A y el vector b
  A <- matrix(0, nrow = n, ncol = n)
  b <- numeric(n)
  
  cat("\nIngrese los valores de la matriz A (coeficientes de las ecuaciones):\n")
  for (i in 1:n) {
    for (j in 1:n) {
      A[i,j] <- as.numeric(readline(prompt = paste("A[", i, ",", j, "]: ", sep = "")))
    }
  }
  
  cat("\nIngrese los valores del vector b (resultados de las ecuaciones):\n")
  for (i in 1:n) {
    b[i] <- as.numeric(readline(prompt = paste("b[", i, "]: ", sep = "")))
  }
  
  # Mostrar la matriz aumentada
  cat("\nLa matriz aumentada [A|b] es:\n")
  Augmented <- cbind(A, b)
  print(Augmented)
  
  # Resolver el sistema de ecuaciones usando Gauss-Jordan
  solution <- gauss_jordan(A, b)
  
  # Mostrar la solución
  cat("\nLa solución del sistema es:\n")
  for (i in 1:n) {
    cat(paste("x", i, " = ", solution[i], "\n", sep = ""))
  }
}

# Llamar al menú para iniciar el programa
menu()
