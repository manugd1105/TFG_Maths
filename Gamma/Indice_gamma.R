long = 0;

for (i in seq(0,1,0.00001)) {
  if (qnorm(i,0,1) >= qnorm(i,7,5)) {
    long = long + 0.00001
  }
}



# Cargar librerías necesarias
library(ggplot2)

# Definir las funciones cuantiles
quantile_N01 <- function(t) {
  qnorm(t, mean = 0, sd = 1)  # N(0,1)
}

quantile_N75 <- function(t) {
  qnorm(t, mean = 7, sd = 5)  # N(7,5)
}

# Generar valores de t entre 0 y 1
t_values <- seq(0, 1, length.out = 1000)

# Calcular los valores de las funciones cuantiles
q_N01 <- sapply(t_values, quantile_N01)
q_N75 <- sapply(t_values, quantile_N75)

# Punto de cruce analítico
crossing_t <- pnorm(-7 / 4)  # t correspondiente al cruce
crossing_q <- quantile_N01(crossing_t)  # Cuantil en N(0,1) o N(7,5), es el mismo

# Crear un data frame para ggplot
data <- data.frame(
  t = t_values,
  Q_N01 = q_N01,
  Q_N75 = q_N75
)

# Graficar las funciones cuantiles
ggplot(data) +
  geom_line(aes(x = t, y = Q_N01, color = "N(0,1)"), size = 1) +
  geom_line(aes(x = t, y = Q_N75, color = "N(7,5)"), size = 1) +
  geom_point(aes(x = crossing_t, y = crossing_q), color = "red", size = 3) +
  annotate("text", x = crossing_t + 0.05, y = crossing_q, 
           label = sprintf("Punto de corte\n(t=%.3f, q=%.2f)", crossing_t, crossing_q), 
           color = "red", vjust = 1, hjust = 0) +
  labs(
    title = "Funciones Cuantiles de N(0,1) y N(7,5)",
    x = "t",
    y = "Cuantil",
    color = "Distribución"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green"))
