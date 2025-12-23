# Fijar semilla para reproducibilidad
set.seed(250)

# Definir tamaño de muestra
n <- 1000

# Generar muestras de N(0,1) y N(7,5)
sample_1 <- rnorm(n, mean = 0, sd = 1)
sample_2 <- rnorm(n, mean = 7, sd = 5)

# Definir las funciones de distribución empirica para las muestras
cdf_sample_1 <- ecdf(sample_1)
cdf_sample_2 <- ecdf(sample_2)

# Crear un rango de valores para t
t_values <- seq(-10, 20, length.out = 10000)

# Calcular las CDF para ambos casos
cdf_values_sample_1 <- cdf_sample_1(t_values)
cdf_values_sample_2 <- cdf_sample_2(t_values)

# Calcular la diferencia entre las dos funciones
difference <- cdf_values_sample_2 - cdf_values_sample_1

# Encontrar el punto donde la diferencia es máxima
max_diff_index <- which.max(difference)
x_max_diff <- t_values[max_diff_index]
y_max_diff_sample_1 <- cdf_values_sample_1[max_diff_index]
y_max_diff_sample_2 <- cdf_values_sample_2[max_diff_index]
max_difference <- difference[max_diff_index]

# Valor de pi
pi_index <- max_difference


# Bootstrap
B <- 10000  # Número de remuestreos
pi_bootstrap <- numeric(B)

for (b in 1:B) {
  # Remuestreo con reemplazo
  boot_sample_1 <- sample(sample_1, n, replace = TRUE)
  boot_sample_2 <- sample(sample_2, n, replace = TRUE)
  
  # Cálculo de cuantiles en las muestras bootstrap
  cdf_sample_1_boot <- ecdf(boot_sample_1)
  cdf_sample_2_boot <- ecdf(boot_sample_2)
  
  cdf_values_sample_1_boot <- cdf_sample_1_boot(t_values)
  cdf_values_sample_2_boot <- cdf_sample_2_boot(t_values)
  
  # Calcular la diferencia entre las dos funciones
  difference <- cdf_values_sample_2_boot - cdf_values_sample_1_boot
  max_diff_index <- which.max(difference)
  max_difference <- difference[max_diff_index]
  
  # Valor de pi
  pi_bootstrap[b] <- max_difference
}

# Transformación de los valores bootstrap
transformed_values <- sqrt(1000 / 2) * (pi_bootstrap - 0.0228)

B1 <- cumsum(rnorm(length(t_values), mean = 0, sd = sqrt(1/length(t_values))))
B1 <- B1 - t_values * B1[length(t_values)]  # Convertir en puente browniano

B2 <- cumsum(rnorm(length(t_values), mean = 0, sd = sqrt(1/length(t_values))))
B2 <- B2 - t_values * B2[length(t_values)]  # Convertir en puente browniano

# 4. Definir lambda y calcular \bar{B}(F_X, F_Y, \lambda)
lambda <- 0.5  # Valor de lambda ajustable
B_bar <- sqrt(lambda) * B1 - sqrt(1 - lambda) * B2

# Graficar la distribución bootstrap transformada
hist(transformed_values, breaks = 40, freq = FALSE, 
     col = "lightblue", border = "black", main = expression(sqrt(1000/2) * (pi[bootstrap] - 0.0228)),
     xlab = "Transformación Bootstrap", ylab = "Densidad")

# Superponer la distribucion de \hat{B}


# Definir el valor de π(F_X, F_Y)
pi_real <- 0.0228  # Valor real de π(F_X, F_Y)

# Identificar el conjunto T
T_values <- t_values[t_values >= pi_real]  # Filtrar valores de t >= π(F_X, F_Y)

# Evaluar el proceso estocástico en los valores de T
B_bar_T <- sqrt(lambda) * B1[t_values >= pi_real] - sqrt(1 - lambda) * B2[t_values >= pi_real]

# Calcular el supremo para cada trayectoria simulada
B_bar_sup <- max(B_bar_T)

# Crear una distribución de valores de B_bar supremo repitiendo la simulación
B_bar_samples <- numeric(10000)

for (i in 1:10000) {
  B1_sim <- cumsum(rnorm(length(t_values), mean = 0, sd = sqrt(1/length(t_values))))
  B1_sim <- B1_sim - t_values * B1_sim[length(t_values)]  # Convertir en puente browniano
  
  B2_sim <- cumsum(rnorm(length(t_values), mean = 0, sd = sqrt(1/length(t_values))))
  B2_sim <- B2_sim - t_values * B2_sim[length(t_values)]  # Convertir en puente browniano
  
  B_bar_sim <- sqrt(lambda) * B1_sim[t_values >= pi_real] - sqrt(1 - lambda) * B2_sim[t_values >= pi_real]
  B_bar_samples[i] <- max(B_bar_sim)  # Supremo
}

# Graficar la distribución bootstrap transformada
hist(transformed_values, breaks = 40, freq = FALSE, 
     col = "lightblue", border = "black", main = expression(sqrt(1000/2) * (pi[bootstrap] - 0.0228)),
     xlab = "Transformación Bootstrap", ylab = "Densidad")

# Dibujar la densidad estimada de B_bar
lines(density(B_bar_samples), col = "red", lwd = 2)

legend("topright", legend = c("Bootstrap Transformado", expression(bar(B)(F[X], F[Y], lambda))), 
       col = c("lightblue", "red"), lwd = 2, bty = "n")

