set.seed(250)

# Parámetros
n <- 1000
m <- 1000
alpha <- 0.05      # Nivel de significación
pi0 <- 0.05        # Valor de contraste para π

# Generar las muestras
sample_1 <- rnorm(n, mean = 0, sd = 1)     # Muestra de N(0,1)
sample_2 <- rnorm(m, mean = 7, sd = 5)     # Muestra de N(7,5)

# Calcular las funciones empíricas
Fn <- ecdf(sample_1)
Gm <- ecdf(sample_2)

# Definir una rejilla con todos los puntos observados
grid <- sort(unique(c(sample_1, sample_2)))

# Bootstrap para estimar la variabilidad de π̂
B <- 10000  # Número de remuestreos
pi_boot <- numeric(B)

for(b in 1:B){
  # Remuestreo con reemplazo
  boot_sample_1 <- sample(sample_1, n, replace = TRUE)
  boot_sample_2 <- sample(sample_2, m, replace = TRUE)
  
  Fn_boot <- ecdf(boot_sample_1)
  Gm_boot <- ecdf(boot_sample_2)
  
  grid_boot <- sort(unique(c(boot_sample_1, boot_sample_2)))
  diffs_boot <- sapply(grid_boot, function(u) Gm_boot(u) - Fn_boot(u))
  
  pi_boot[b] <- max(diffs_boot)
}

# Estimador puntual de π a partir del bootstrap
pi_hat <- mean(pi_boot)

# Factor de escalado asintótico: √(nm/(n+m))
scale_factor <- sqrt(n * m / (n + m))  # Para n = m, equivale a √(n/2)

# Transformamos las replicaciones bootstrap para estimar la varianza
transformed_values <- scale_factor * (pi_boot - pi_hat)
sigma_hat <- sd(transformed_values)

cat("Estimación bootstrap de σ (sigma_hat) =", sigma_hat, "\n")

# Estadístico de prueba
H <- scale_factor * (pi_hat - pi0) / sigma_hat
crit_val <- qnorm(alpha)

cat("Estadístico de prueba H =", H, "\n")
cat("Valor crítico qnorm(alpha) =", crit_val, "\n")

if (H < crit_val) {
  cat("=> Se RECHAZA H0: hay evidencia de que π(F,G) < ", pi0, "\n")
} else {
  cat("=> NO se rechaza H0: no hay evidencia suficiente de que π(F,G) < ", pi0, "\n")
}
