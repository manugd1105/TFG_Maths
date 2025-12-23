# Función cuantil empírica escalera
empirical_quantile <- function(values, p) {
  n_val <- length(values)
  sorted_values <- sort(values)
  np <- n_val * p
  
  # Si np es entero, usar el valor en esa posición; si no, el siguiente mayor
  quantiles <- sapply(np, function(k) {
    if (k %% 1 == 0) {
      sorted_values[k]
    } else {
      sorted_values[ceiling(k)]
    }
  })
  return(quantiles)
}