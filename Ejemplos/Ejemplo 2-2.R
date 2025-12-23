# Instalar y cargar los paquetes necesarios
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Definir los parámetros de las dos normales
mu1 <- 0
sigma1 <- 1
mu2 <- 3
sigma2 <- 1

# Crear una secuencia de valores de x para evaluar las CDFs
x_values <- seq(-5, 10, length.out = 1000)

# Calcular las CDFs para ambas normales
cdf_X1 <- pnorm(x_values, mean = mu1, sd = sigma1)
cdf_X2 <- pnorm(x_values, mean = mu2, sd = sigma2)

# Crear un data frame para las CDFs
data <- data.frame(
  x = rep(x_values, 2),
  cdf = c(cdf_X1, cdf_X2),
  Distribución = rep(c("X ~ N(0, 1)", "Y ~ N(3, 1)"), each = length(x_values))
)

# Graficar las funciones de distribución acumulada
ggplot(data, aes(x = x, y = cdf, color = Distribución)) +
  geom_line(size = 1.2) +
  labs(title = "Funciones de distribución: N(0,1) y N(3,1)",
       x = "x",
       y = "Distribución Acumulada") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

