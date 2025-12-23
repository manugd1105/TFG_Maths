# Fijar semilla para reproducibilidad
set.seed(250)

# Definir tamaño de muestra
n <- 1000

gamma_index <- pnorm(-7/4)

# Estimaciones con muestras
B <- 10000  # Número de muestras
gamma_estimado <- numeric(B)

for (b in 1:B) {
  sample1 <- rnorm(n, mean = 0, sd = 1)
  sample2 <- rnorm(n, mean = 7, sd = 5)
  
  sort1 <- sort(sample1)
  sort2 <- sort(sample2)
  
  l <- sort1>sort2
  
  gamma_estimado[b] <- mean(l)
}

# Transformación de los valores bootstrap
transformed_values <- sqrt(1000 / 2) * (gamma_estimado - gamma_index)

# Graficar la distribución bootstrap transformada
hist(transformed_values, breaks = 40, freq = FALSE, 
     col = "lightblue", border = "black", main = expression(sqrt(1000/2) * (hat(gamma) - gamma(F[X],F[Y]))),
     xlab = "Transformación Bootstrap", ylab = "Densidad")

# Superponer la densidad de la normal
x_vals <- seq(min(transformed_values), max(transformed_values), length.out = 2000)
normal_density <- dnorm(x_vals, mean = mean(transformed_values), sd = sd(transformed_values))

lines(x_vals, normal_density, col = "red", lwd = 2)
legend("topright", legend = "Densidad N(0.0005, 0.1758)", col = "red", lwd = 2)

# Mostrar estadísticas del bootstrap
gamma_mean <- mean(gamma_estimado)
gamma_sd <- sd(gamma_estimado)
gamma_ci <- quantile(gamma_estimado, c(0.025, 0.975))

trans_mean <- mean(transformed_values)
trans_sd <- sd(transformed_values)
trans_ci <- quantile(transformed_values, c(0.025, 0.975))

cat("Transformed Mean:", trans_mean, "\n")
cat("Transformed Std Dev:", trans_sd, "\n")
cat("95% CI:", gamma_ci[1], "-", gamma_ci[2], "\n")











# ── Cargar paquete ──────────────────────────────────────────────────────────────
library(ggplot2)

# Pasamos los valores transformados a un data-frame para ggplot
df <- data.frame(trans = transformed_values)

# Media y desviación estándar de la transformación  (las usaremos en la curva)
m   <- mean(df$trans)
s   <- sd(df$trans)

# ── Gráfico ─────────────────────────────────────────────────────────────────────
ggplot(df, aes(x = trans)) +
  # Histograma a densidad
  geom_histogram(aes(y = ..density.., fill = "Bootstrap"),
                 bins = 32, colour = "black", alpha = 0.5) +
  # Curva Normal teórica                (usa stat_function → no hace falta pre-calcular)
  stat_function(fun  = dnorm,
                args = list(mean = m, sd = s),
                aes(colour = "Normal teórica"),
                linewidth = 1.1) +
  # Etiquetas y títulos
  labs(
    title = expression(sqrt(1000/2) * (hat(gamma) - gamma(F[X], F[Y]))),
    x     = expression(hat(gamma)),
    y     = "Densidad"
  ) +
  # Leyenda manual (así controlas colores y orden)
  scale_fill_manual(
    name   = "",           # sin título de leyenda
    values = c("Bootstrap" = "lightblue"),
    labels = c("Bootstrap" = "Histograma")
  ) +
  scale_color_manual(
    name   = "",
    values = c("Normal teórica" = "red"),
    labels = c("Normal teórica" = "Distribución N(0.0005, 0.1758)")
  ) +
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) # coloca la leyenda arriba (o “right”, “none”...)
  

