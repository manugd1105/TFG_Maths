# Fijar la semilla
set.seed(250)

# Generar las dos muestras
X <- rnorm(1000, mean = 0, sd = 1)
Y <- rnorm(1000, mean = 7, sd = 5)


# Fun dist emp
Fx <- ecdf(X)
Fy <- ecdf(Y)

# Encontrar el punto de corte (x_est) entre Fx y Fy
x_range <- sort(unique(c(X, Y)))
dif_F <- abs(Fx(x_range) - Fy(x_range))
x_est <- x_range[which.min(dif_F)]


# Cuantiles y gamma_est

########
#Ordenar las muestras

x_sorted <- sort(X)
y_sorted <- sort(Y)
n <- length(X)
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
p_query <- (1:1000)/1001
x_quantiles <- empirical_quantile(x_sorted, p_query)
y_quantiles <- empirical_quantile(y_sorted, p_query)

# Encontrar el punto de intersección
diff <- abs(x_quantiles - y_quantiles)
min_index <- which.min(diff)
intersection_p <- p_query[min_index]
intersection_value <- x_quantiles[min_index]
########
gamma_est <- intersection_p

gamma_est <- mean(x_sorted>y_sorted)



















# Ahora, las funciones de densidad empiricas

# Estimar las densidades empíricas en x_est con densidades kernel
density_X <- density(X, from = mean(X)-abs(x_est), to = mean(X)+abs(x_est))
density_Y <- density(Y, from = mean(X)-abs(x_est), to = mean(X)+abs(x_est))

# Interpolar las densidades en x_est
fX_est <- approx(density_X$x, density_X$y, xout = x_est)$y
# fX_est <- 10^(-38)
fY_est <- approx(density_Y$x, density_Y$y, xout = x_est)$y

# Finalmente, se calcula sigma^{2}

lambda <- length(Y) / (length(X) + length(Y))

gamma_star <- mean(sort(X)>sort(Y))

sigma_squared <- (gamma_star * (1 - gamma_star) * ((1 - lambda) * fY_est^2 + lambda * fX_est^2)) / 
  (fY_est - fX_est)^2

sigma_squared
sqrt(sigma_squared)

