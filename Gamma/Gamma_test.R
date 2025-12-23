# Crear la función cuantil empírica escalera
empirical_quantile <- function(values, p_query) {
  n <- length(values)
  sorted_values <- sort(values)
  np <- n * p_query
  
  # Si np es entero, usar el valor en esa posición, si no, el siguiente mayor
  quantiles <- sapply(np, function(k) {
    if (k %% 1 == 0) {
      sorted_values[k]
    } else {
      sorted_values[ceiling(k)]
    }
  })
  return(quantiles)
}



# Test gamma

gamma_0 <- 0.05
alpha <- 0.05

set.seed(250)

# Definir tamaño de muestra
n <- 1000

# Generar muestras de N(0,1) y N(7,5)
sample_1 <- rnorm(n, mean = 0, sd = 1)
sample_2 <- rnorm(n, mean = 7, sd = 5)

# Bootstrap
B <- 10000  # Número de remuestreos
gamma_bootstrap <- numeric(B)
p_query <- (1:1000)/1001

for (b in 1:B) {
  # Remuestreo con reemplazo
  boot_sample_1 <- sample(sample_1, n, replace = TRUE)
  boot_sample_2 <- sample(sample_2, n, replace = TRUE)
  
  # Cálculo de cuantiles en las muestras bootstrap
  sample_1_boot_quantiles <- empirical_quantile(sort(boot_sample_1), p_query)
  sample_2_boot_quantiles <- empirical_quantile(sort(boot_sample_2), p_query)
  
  # Encontrar el punto de intersección
  diff <- abs(sample_1_boot_quantiles - sample_2_boot_quantiles)
  min_index <- which.min(diff)
  intersection_p <- p_query[min_index]
  intersection_value <- sample_1_boot_quantiles[min_index]
  
  # Valor de gamma
  gamma_bootstrap[b] <- intersection_p
}

gamma_hat <- mean(gamma_bootstrap)

transformed_values <- sqrt(1000 / 2) * (gamma_bootstrap - gamma_hat)

sigma_bootstrap_sd <- sd(transformed_values)

H <- sqrt(1000 / 2)*(gamma_hat - gamma_0) / sigma_bootstrap_sd

crit_val <- qnorm(alpha)

cat("Estadístico de prueba H =", H, "\n")
cat("Valor crítico qnorm(alpha) =", crit_val, "\n")

if (H < crit_val) {
  cat("=> Se RECHAZA H0: hay evidencia de que π(F,G) < ", gamma_0, "\n")
} else {
  cat("=> NO se rechaza H0: no hay evidencia suficiente de que π(F,G) < ", gamma_0, "\n")
}


