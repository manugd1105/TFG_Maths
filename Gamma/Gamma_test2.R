# Función para obtener el cuantil empírico "escalera"
empirical_quantile <- function(values, p_query) {
  sorted_values <- sort(values)
  n <- length(sorted_values)
  np <- n * p_query
  quantiles <- sapply(np, function(k) {
    if (k %% 1 == 0) {
      sorted_values[k]
    } else {
      sorted_values[ceiling(k)]
    }
  })
  return(quantiles)
}

# Parámetros del test
gamma_0 <- 0.05    # Valor a contrastar
alpha <- 0.05      # Nivel de significación
set.seed(250)
n <- 1000          # Tamaño de muestra (para ambas muestras)

# Generar las muestras: sample_1 ~ N(0,1) y sample_2 ~ N(7,5)
sample_1 <- rnorm(n, mean = 0, sd = 1)
sample_2 <- rnorm(n, mean = 7, sd = 5)

# Bootstrap
B <- 10000         # Número de remuestreos
gamma_bootstrap <- numeric(B)
p_query <- (1:n) / (n + 1)   # Secuencia de probabilidades

for (b in 1:B) {
  # Remuestreo con reemplazo
  boot_sample_1 <- sample(sample_1, n, replace = TRUE)
  boot_sample_2 <- sample(sample_2, n, replace = TRUE)
  
  # Calcular cuantiles empíricos para cada remuestra
  quantiles1 <- empirical_quantile(boot_sample_1, p_query)
  quantiles2 <- empirical_quantile(boot_sample_2, p_query)
  
  # Se busca el índice en el que la diferencia entre cuantiles es mínima
  diff_vec <- quantiles1 - quantiles2
  min_index <- which.min(abs(diff_vec))
  candidate <- p_query[min_index]
  
  # Según el artículo, el estimador de γ se define:
  #   si la diferencia en el punto de cruce es ≥ 0, se toma candidate,
  #   y si es < 0, se toma 1 - candidate.
  if (diff_vec[min_index] >= 0) {
    gamma_bootstrap[b] <- candidate
  } else {
    gamma_bootstrap[b] <- 1 - candidate
  }
}

# Estimador puntual de γ a partir del bootstrap
gamma_hat <- mean(gamma_bootstrap)

# Para construir el estadístico de prueba se estima la varianza de γ_hat.
# Con n = m, el factor de escalado es √(n/2)
transformed_values <- sqrt(n / 2) * (gamma_bootstrap - gamma_hat)
sigma_hat <- sd(transformed_values)

# Estadístico de prueba:
H <- sqrt(n / 2) * (gamma_hat - gamma_0) / sigma_hat

# Valor crítico para test unilateral (cola inferior) al nivel α
crit_val <- qnorm(alpha)

cat("Estadístico de prueba H =", H, "\n")
cat("Valor crítico qnorm(alpha) =", crit_val, "\n")

if (H < crit_val) {
  cat("=> Se RECHAZA H0: hay evidencia de que γ(F,G) < ", gamma_0, "\n")
} else {
  cat("=> NO se rechaza H0: no hay evidencia suficiente de que γ(F,G) < ", gamma_0, "\n")
}
