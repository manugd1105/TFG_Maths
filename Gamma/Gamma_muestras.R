
# Cargar ggplot2
library(ggplot2)

# Crear una secuencia de probabilidades (valores entre 0 y 1)
probs <- seq(0, 1, length.out = 100)

# Calcular las funciones cuantil (inversas)
quantile_normal <- qnorm(probs, mean = 0, sd = 1)   # Normal(0,1)
quantile_exp10 <- qexp(probs, rate = 10)            # Exponencial con lambda = 10

# Crear un data frame para ggplot
data <- data.frame(
  Probability = probs,
  Quantile_Normal = quantile_normal,
  Quantile_Exp10 = quantile_exp10
)

# Graficar con ggplot2
ggplot(data, aes(x = Probability)) +
  geom_line(aes(y = Quantile_Normal, color = "Normal (0,1)"), size = 1) +
  geom_line(aes(y = Quantile_Exp10, color = "Exponencial λ=10"), size = 1) +
  labs(
    title = "Funciones cuantil de una normal y una exponencial",
    x = "Probabilidad acumulada",
    y = "Cuantil",
    color = "Distribución"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

#Cortes:

# Función para calcular la diferencia entre las funciones cuantil
diff_quantile <- function(p) {
  qnorm(p, mean = 0, sd = 1) - qexp(p, rate = 10)
}

# Encontrar los puntos de corte
root1 <- uniroot(diff_quantile, interval = c(0, 0.6))$root
root2 <- uniroot(diff_quantile, interval = c(0.6, 1))$root

# Calcular los valores cuantil en los puntos de corte
quantile1 <- qnorm(root1, mean = 0, sd = 1)
quantile2 <- qnorm(root2, mean = 0, sd = 1)

# Resultados
cat("Primer punto de corte:\n")
cat("Probabilidad:", root1, "\tCuantil:", quantile1, "\n")
cat("Segundo punto de corte:\n")
cat("Probabilidad:", root2, "\tCuantil:", quantile2, "\n")








## Dos muestras
# options(digits=1)
set.seed(250)
x <- rnorm(30,mean=0,sd=1)
y <- rnorm(30,mean=7,sd=5)

samples <- list(x,y)



# Ordenar las muestras
x_sorted <- sort(x)
y_sorted <- sort(y)
n <- length(x)
p <- (1:n) / (n + 1) # Probabilidades empíricas

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

# Evaluar las funciones cuantil empíricas para un rango de p
p_query <- (1:30)/31
x_quantiles <- empirical_quantile(x_sorted, p_query)
y_quantiles <- empirical_quantile(y_sorted, p_query)

# Encontrar el punto de intersección
diff <- abs(x_quantiles - y_quantiles)
min_index <- which.min(diff)
intersection_p <- p_query[min_index]
intersection_value <- x_quantiles[min_index]

df <- data.frame(
  p = rep(p_query, 2),
  value = c(x_quantiles, y_quantiles),
  group = rep(c("Muestra X", "Muestra Y"), each = length(p_query))
)

library(ggplot2)

# Gráfico con ggplot
ggplot(df, aes(x = p, y = value, color = group)) +
  geom_step(size = 1.2) + # Funciones cuantil en escalera
  geom_vline(xintercept = intersection_p, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = intersection_value, linetype = "dashed", color = "gray") +
  geom_point(aes(x = intersection_p, y = intersection_value), color = "red", size = 3) +
  annotate("text", x = intersection_p + 0.05, y = intersection_value - 0.2, 
           label = paste0("(", round(intersection_p, 3), ", ", round(intersection_value, 3), ")"), 
           color = "red", hjust = 0.2, vjust = 1) +
  labs(
    title = "Funciones cuantil empíricas",
    x = "Probabilidad empírica (p)",
    y = "Valores"
  ) +
  scale_color_manual(values = c("blue", "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Imprimir el punto de corte
list(Probabilidad = intersection_p, Valor = intersection_value)


