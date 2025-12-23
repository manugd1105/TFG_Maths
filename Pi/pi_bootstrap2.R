# ==============================================
# 1. Simulación de datos
# ==============================================
set.seed(250)
n <- 1000  
m <- 1000  

X <- rnorm(n, mean = 0, sd = 1)
Y <- rnorm(m, mean = 7, sd = 5)

# Funciones de distribución empíricas
Fx <- ecdf(X)
Fy <- ecdf(Y)

# ==============================================
# 2. Cálculo de π(F_X, F_Y)
# ==============================================
t_values <- seq(min(c(X, Y)), max(c(X, Y)), length.out = 10000)
diff_cdf <- Fy(t_values) - Fx(t_values)

# Encontrar el valor máximo de la diferencia de distribuciones
pi_real <- max(diff_cdf)

# ==============================================
# 3. Bootstrap para π
# ==============================================
B <- 10000  
pi_bootstrap <- numeric(B)

for (b in 1:B) {
  X_boot <- sample(X, n, replace = TRUE)
  Y_boot <- sample(Y, m, replace = TRUE)
  
  Fx_boot <- ecdf(X_boot)
  Fy_boot <- ecdf(Y_boot)
  
  diff_boot <- Fy_boot(t_values) - Fx_boot(t_values)
  pi_bootstrap[b] <- max(diff_boot)
}

# Transformación bootstrap
transformed_values <- sqrt(n * m / (n + m)) * (pi_bootstrap - pi_real)

# ==============================================
# 4. Simulación de la distribución límite \bar{B}
# ==============================================

# Función para simular puentes Brownianos
simulate_brownian_bridge <- function(t) {
  W <- cumsum(rnorm(length(t), mean = 0, sd = sqrt(1/length(t))))
  return(W - t * W[length(t)])  # Corrección para cumplir las condiciones del puente
}

lambda <- n / (n + m)  

B_bar_samples <- numeric(B)

for (i in 1:B) {
  B1 <- simulate_brownian_bridge(t_values)
  B2 <- simulate_brownian_bridge(t_values)
  
  # Definir correctamente el conjunto T
  T_values <- t_values[t_values >= pi_real & t_values <= 1]
  
  # Evaluar el proceso en T
  valid_indices <- which(t_values >= pi_real & t_values <= 1)
  B_bar_T <- sqrt(lambda) * B1[valid_indices] - sqrt(1 - lambda) * B2[valid_indices]
  
  # Verificar que hay valores en T antes de tomar el supremo
  if (length(B_bar_T) > 0) {
    B_bar_samples[i] <- max(B_bar_T)
  }
}

# ==============================================
# 5. Verificación de la distribución antes del ajuste
# ==============================================
par(mfrow = c(1, 2))  
hist(transformed_values, breaks = 40, freq = FALSE, col = "lightblue", main = "Bootstrap Transformado")
hist(B_bar_samples, breaks = 40, freq = FALSE, col = "red", main = "B_bar sin normalizar")

# ==============================================
# 6. Ajuste preciso de la escala de B_bar
# ==============================================
mean_bootstrap <- mean(transformed_values)
sd_bootstrap <- sd(transformed_values)
mean_Bbar <- mean(B_bar_samples)
sd_Bbar <- sd(B_bar_samples)

B_bar_samples_adjusted <- ((B_bar_samples - mean_Bbar) / sd_Bbar) * sd_bootstrap + mean_bootstrap

# ==============================================
# 7. Comparación gráfica final
# ==============================================
par(mfrow = c(1, 1))  

hist(transformed_values, breaks = 40, freq = FALSE, 
     col = "lightblue", border = "black", main = expression(sqrt(nm/(n+m)) * (pi[bootstrap] - pi_real)),
     xlab = "Transformación Bootstrap", ylab = "Densidad")

# Superponer la densidad de B_bar ajustado
lines(density(B_bar_samples_adjusted, adjust = 1.2), col = "red", lwd = 2)

legend("topright", legend = c("Bootstrap Transformado", expression(bar(B)(F[X], F[Y], lambda))), 
       col = c("lightblue", "red"), lwd = 2, bty = "n")
