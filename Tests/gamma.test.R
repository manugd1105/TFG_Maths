#gamma.test <- function(sample1, sample2, gamma0, 
#                      alternative = c("two.sided", "greater", "less"), 
#                       B = 10000, boot = TRUE) {
  # B es el número de muestras bootstrap
  # boot es si se hace por bootstrap o por la estimacion de las fun de dens kernel
  
  # Validar la alternativa
  #alternative <- match.arg(alternative)

gamma.test <- function(sample1, sample2, gamma0, alternative = "less", 
                       B = 10000, boot = TRUE) {
  
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  
  # Tamaños de muestra (posiblemente distintos)
  n <- length(sample1)
  m <- length(sample2)
  
  # Definir r como en la funcion compute_r
  source("compute_r.R")
  r <- compute_r(n,m)
  p <- (1:r) / (r + 1)
  
  # Llamada a la funcion quantil empirica (tenerla en el mismo directorio)
  source("empirical_quantile.R")
  
  # Factor de escalado
  scale_factor <- sqrt(n * m / (n + m))
  
  if (boot) {
    # ---------------------------
    # Se sigue el procedimiento por Bootstrap
    # ---------------------------
    gamma_bootstrap <- numeric(B)
    
    for (b in 1:B) {
      # Remuestreo con reemplazo
      boot_sample1 <- sample(sample1, n, replace = TRUE)
      boot_sample2 <- sample(sample2, m, replace = TRUE)
      
      # Cálculo de cuantiles en las muestras bootstrap
      sample_1_boot_quantiles <- empirical_quantile(sort(boot_sample1), p)
      sample_2_boot_quantiles <- empirical_quantile(sort(boot_sample2), p)
      
      # Encontrar el punto de intersección: el p donde la diferencia absoluta es mínima
      #diff <- abs(sample_1_boot_quantiles - sample_2_boot_quantiles)
      #min_index <- which.min(diff)
      #intersection_p <- p[min_index]
      
      # Lo hago con la proporcion de puntos mayores
      sort1 <- sort(boot_sample1)
      sort2 <- sort(boot_sample2)
      
      l <- sort1>sort2
      
      # Almacenar el valor
      #gamma_bootstrap[b] <- intersection_p
      
      gamma_bootstrap[b] <- mean(l)
    }
    
    # Estimador puntual de gamma: estumacion con las muestras originales
    sort1 <- sort(sample1)
    sort2 <- sort(sample2)
    gamma_hat <- mean(sort1>sort2)
    
    
    # Transformar los valores bootstrap
    transformed_values <- scale_factor * (gamma_bootstrap - gamma_hat)
    sigma_hat <- sd(transformed_values)
    
  } else {
    # ---------------------------
    # Cálculo de sigma con las fun de dens kernel
    # ---------------------------
    # 1. Calcular gamma_hat mediante la intersección de cuantiles
    sample1_sorted <- sort(sample1)
    sample2_sorted <- sort(sample2)
    # q1 <- empirical_quantile(sample1_sorted, p)
    # q2 <- empirical_quantile(sample2_sorted, p)
    # diff_quantiles <- abs(q1 - q2)
    # min_index <- which.min(diff_quantiles)
    # gamma_hat <- p[min_index]
    
    l <- sample1_sorted > sample2_sorted
    
    # Almacenar el valor
    #gamma_bootstrap[b] <- intersection_p
    
    gamma_hat <- mean(l)
    
    # 2. Calcular x_est a partir de las funciones de distribución empíricas
    Fx <- ecdf(sample1)
    Fy <- ecdf(sample2)
    x_range <- sort(unique(c(sample1, sample2)))
    dif_F <- abs(Fx(x_range) - Fy(x_range))
    x_est <- x_range[which.min(dif_F)]
    
    # 3. Estimar las densidades empíricas en x_est mediante estimación kernel
    density1 <- density(sample1, from = min(x_est,min(sample1)), to = max(x_est,max(sample1)))
    density2 <- density(sample2, from = min(x_est,min(sample2)), to = max(x_est,max(sample2)))
    f1_est <- approx(density1$x, density1$y, xout = x_est)$y
    f2_est <- approx(density2$x, density2$y, xout = x_est)$y
    
    # 4. Calcular lambda y sigma^2 siguiendo el articulo
    lambda <- m / (n + m)
    sigma_squared <- (gamma_hat * (1 - gamma_hat) * (((1 - lambda) * f2_est^2) + (lambda * f1_est^2))) / 
      (f2_est - f1_est)^2
    sigma_hat <- sqrt(sigma_squared)
  }
  
  # Estadístico de prueba
  H <- scale_factor * (gamma_hat - gamma0) / sigma_hat
  
  # p-value según la alternativa, asumiendo aproximación normal asintótica
  if (alternative == "less") {
    p_value <- pnorm(H)
  } else if (alternative == "greater") {
    p_value <- 1 - pnorm(H)
  } else if (alternative == "two.sided") {  # two.sided
    p_value <- 2 * pnorm(-abs(H))
  }
  
  # Devolver resultados en una lista
  return(list(statistic = H, 
              p_value = p_value, 
              gamma = gamma_hat))
}
