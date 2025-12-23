# Cargar librerías necesarias
library(ggplot2)
library(tidyr)

# Crear el rango de x entre -2 y 5
x_unif1 <- seq(-2, 5, length.out = 1000)
y_unif1 <- punif(x_unif1, min = 0, max = 1)  # Uniforme entre 0 y 1

# Crear los valores de la distribución unif entre 2 y 3
x_unif2 <- seq(-2, 5, length.out = 1000)
y_unif2 <- punif(x_unif2, min = 2, max = 3)  # Uniforme entre 2 y 3

# Convertimos los datos a un formato long para ggplot2
data_unif1 <- data.frame(x = x_unif1, y = y_unif1, Distribucion = "X ~ U(0, 1)")
data_unif2 <- data.frame(x = x_unif2, y = y_unif2, Distribucion = "Y ~ U(2, 3)")

# Unimos los dataframes
data <- rbind(data_unif1, data_unif2)

# Graficamos con ggplot2
ggplot(data, aes(x = x, y = y, color = Distribucion)) +
  geom_line(data = data_unif1, size = 1.2) +   
  geom_step(data = data_unif2, size = 1.2) + 
  labs(x = "x", y = "Distribución Acumulada", 
       title = "Funciones de distribución: U(0,1) y U(2,3)") +
  scale_color_manual(values = c("blue", "red")) +  # Colores personalizados
  theme_minimal()                                  # Tema limpio y minimalista
