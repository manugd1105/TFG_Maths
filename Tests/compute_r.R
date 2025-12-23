# Función para calcular el mcd usando el algoritmo de Euclides
mcd <- function(a, b) {
  if (b == 0) return(a)
  return(mcd(b, a %% b))
}

# Función para calcular el lcm usando la relación: mcm(a, b) = |a * b| / gcd(a, b)
mcm <- function(a, b) {
  return(abs(a * b) / mcd(a, b))
}

# Función principal que devuelve min(n, m) si uno es múltiplo del otro, 
# o el mcm en caso contrario.
compute_r <- function(n, m) {
  if ((n %% m == 0) || (m %% n == 0)) {
    return(min(n, m))
  } else {
    return(mcm(n, m))
  }
}