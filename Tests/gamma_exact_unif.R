gamma_exact_unif <- function(muX, sigmaX, muY, sigmaY, tol = 1e-12) {
  # Parámetros válidos
  if (sigmaX <= 0 || sigmaY <= 0) {
    stop("sigmaX y sigmaY deben ser positivos")
  }
  # Extremos de cada uniforme
  aX <- muX - sqrt(3) * sigmaX
  bX <- muX + sqrt(3) * sigmaX
  aY <- muY - sqrt(3) * sigmaY
  bY <- muY + sqrt(3) * sigmaY
  
  # Diferencia de longitudes
  delta <- (bX - aX) - (bY - aY)
  
  # Caso casi iguales
  if (abs(delta) < tol) {
    return(if (muX > muY) 1 else 0)
  }
  
  # Punto de cruce
  tstar <- (aY - aX) / delta
  
  # Cálculo de gamma según signo de delta y posición de tstar
  if (delta > 0) {
    gamma <- if (tstar <= 0) {
      1
    } else if (tstar < 1) {
      1 - tstar
    } else {
      0
    }
  } else {
    gamma <- if (tstar <= 0) {
      0
    } else if (tstar < 1) {
      tstar
    } else {
      1
    }
  }
}