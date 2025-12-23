# Cargar las librerías necesarias
library(ggplot2)

# Generar muestras
set.seed(250)
x <- rnorm(30,0,1)
y <- rnorm(30,7,5)

# Definir las funciones de distribución empirica para las muestras
cdf_x <- ecdf(x)
cdf_y <- ecdf(y)

# Crear un rango de valores para t
t_values <- seq(-10, 20, length.out = 10000)

# Calcular las CDF para ambos casos
cdf_values_x <- cdf_x(t_values)
cdf_values_y <- cdf_y(t_values)

# Calcular la diferencia entre las dos funciones
difference <- cdf_values_y - cdf_values_x

# Encontrar el punto donde la diferencia es máxima
max_diff_index <- which.max(difference)
x_max_diff <- t_values[max_diff_index]
y_max_diff_x <- cdf_values_x[max_diff_index]
y_max_diff_y <- cdf_values_y[max_diff_index]
max_difference <- difference[max_diff_index]

# Crear la gráfica
ggplot() +
  geom_line(aes(x = t_values, y = cdf_values_x, color = "Muestra X"), size = 1) +
  geom_line(aes(x = t_values, y = cdf_values_y, color = "Muestra Y"), size = 1) +
  geom_point(aes(x = x_max_diff, y = y_max_diff_x), color = "red", size = 3) +
  geom_point(aes(x = x_max_diff, y = y_max_diff_y), color = "red", size = 3) +
  geom_segment(aes(x = x_max_diff, xend = x_max_diff, 
                   y = y_max_diff_x, yend = y_max_diff_y), 
               color = "black", linetype = "dashed") +
  annotate("text", x = x_max_diff, y = (y_max_diff_x + y_max_diff_y) / 2, 
           label = paste("Máx en t =", round(x_max_diff, 2), 
                         "\nDiferencia =", round(max_difference, 3)), 
           vjust = -0.8, hjust = 0.8, size = 3.5, color = "red") +
  labs(title = "Función de distribución empírica de X e Y",
       x = "t", y = "Función de Distribución Empírica") +
  scale_color_manual(values = c("Muestra X" = "blue", "Muestra Y" = "green"),
                     name = "Distribuciones") +
  theme_minimal()

# Imprimir el punto de máxima diferencia
cat("La máxima diferencia ocurre en t =", x_max_diff, 
    "con una diferencia de", difference[max_diff_index], "\n")
