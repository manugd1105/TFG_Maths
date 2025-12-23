# Cargar las librerías necesarias
library(ggplot2)

# Definir las funciones de distribución acumulativa (CDF) para N(0,1) y N(7,5)
cdf_N01 <- function(x) pnorm(x, mean = 0, sd = 1)
cdf_N75 <- function(x) pnorm(x, mean = 7, sd = 5)

# Crear un rango de valores para x
x_values <- seq(-10, 20, length.out = 10000)

# Calcular las CDF para ambos casos
cdf_values_N01 <- cdf_N01(x_values)
cdf_values_N75 <- cdf_N75(x_values)

# Calcular la diferencia entre las dos funciones
difference <- cdf_values_N75 - cdf_values_N01

# Encontrar el punto donde la diferencia es máxima
max_diff_index <- which.max(difference)
x_max_diff <- x_values[max_diff_index]
y_max_diff_N01 <- cdf_values_N01[max_diff_index]
y_max_diff_N75 <- cdf_values_N75[max_diff_index]
max_difference <- difference[max_diff_index]

# Crear la gráfica
ggplot() +
  geom_line(aes(x = x_values, y = cdf_values_N01, color = "N(0,1)"), size = 1) +
  geom_line(aes(x = x_values, y = cdf_values_N75, color = "N(7,5)"), size = 1) +
  geom_point(aes(x = x_max_diff, y = y_max_diff_N01), color = "red", size = 3) +
  geom_point(aes(x = x_max_diff, y = y_max_diff_N75), color = "red", size = 3) +
  geom_segment(aes(x = x_max_diff, xend = x_max_diff, 
                   y = y_max_diff_N01, yend = y_max_diff_N75), 
               color = "black", linetype = "dashed") +
  annotate("text", x = x_max_diff, y = (y_max_diff_N01 + y_max_diff_N75) / 2, 
           label = paste("Máx en t =", round(x_max_diff, 2), 
                         "\nDiferencia =", round(max_difference, 10)), 
           vjust = -1, size = 3.5, color = "red") +
  labs(title = "Función de distribución de N(0,1) y N(7,5)",
       x = "t", y = "Función de Distribución Acumulada (CDF)") +
  scale_color_manual(values = c("N(0,1)" = "blue", "N(7,5)" = "green"),
                     name = "Distribuciones") +
  theme_minimal()

# Imprimir el punto de máxima diferencia
cat("La máxima diferencia ocurre en x =", x_max_diff, 
    "con una diferencia de", difference[max_diff_index], "\n")
