pi.test <- function(sample1, sample2, pi0, alternative = "less", 
                    B = 10000) {
  
  # Para comprobar que el argumento de entrada sea valido
  alternative <- match.arg(alternative, c("two.sided", "greater", "less")) 
  
  # Tamaños de muestra
  n <- length(sample1)
  m <- length(sample2)
  
  # Factor de escalado: para muestras iguales, sqrt(n/2); en general sqrt(n*m/(n+m))
  scale_factor <- sqrt(n * m / (n + m))
  
  # Función para calcular el máximo de (G(x)-F(x)) en una rejilla de puntos
  # Se utiliza la función ecdf sobre la rejilla de puntos
  source("compute_pi.R")
  
  # Bootstrap para estimar la distribución de pi
  pi_boot <- numeric(B)
  for(b in 1:B) {
    boot_sample1 <- sample(sample1, n, replace = TRUE)
    boot_sample2 <- sample(sample2, m, replace = TRUE)
    grid_boot <- sort(unique(c(boot_sample1, boot_sample2)))
    pi_boot[b] <- compute_pi(boot_sample1, boot_sample2, grid = grid_boot)
  }
  
  # Estimador puntual de pi: se toma la estimacion con las muestras originales
  pi_hat <- compute_pi(sample1, sample2)
  
  # Se transforman las replicaciones mediante el factor de escalado y se calcula la sd
  transformed_values <- scale_factor * (pi_boot - pi_hat)
  sigma_hat <- sd(transformed_values)
  
  # Estadístico de prueba
  H <- scale_factor * (pi_hat - pi0) / sigma_hat
  
  # p-value según la alternativa, asumiendo que H se aproxima a una N(0,1)
  if(alternative == "less") {
    p_value <- pnorm(H)
  } else if(alternative == "greater") {
    p_value <- 1 - pnorm(H)
  } else if(alternative == "two.sided") {
    p_value <- 2 * min(pnorm(H), 1 - pnorm(H))
  }
  
  # Devolver una lista con el estadístico y el p-value
  return(list(statistic = H, p_value = p_value, pi = pi_hat))
}
