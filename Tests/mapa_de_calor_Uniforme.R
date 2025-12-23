# Para el gamma

inicio <- Sys.time()
library(tidyverse)
library(scales)

source("gamma.test.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
gamma0_less  <- 0.1
gamma0_greater <- 0.9
alpha   <- 0.05  # Nivel de significación
B_boot  <- 500   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.1
df.heatmapN <- expand.grid(mu = seq(-10,10,paso), sigma = seq(paso,5,paso))

# Para las potencias

Potencia <- function(mu,sigma){
  rechazos <- replicate(n_rep,{
    Nest <- runif(n, min = - sqrt(3), max = sqrt(3))
    a <- mu - sqrt(3) * sigma
    b <- mu + sqrt(3) * sigma
    N <- runif(n, min = a, max = b)
    gamma.test(Nest,N,gamma0_greater,boot=F,B=B_boot,alternative="greater")$p_value<=alpha
  })
  return(mean(rechazos))
}
Potencia <- Vectorize(Potencia)
df.heatmapN$R <-Potencia(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  scale_fill_gradientn(
    colors = c("green", "white", "red"),
    values = rescale(c(0, 0.05, 1)),
    limits = c(0, 1)
  ) +
  #scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = alpha) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)



# Para el pi

inicio <- Sys.time()
library(tidyverse)

source("pi.test.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
pi0  <- 0.1
alpha   <- 0.05  # Nivel de significación
B_boot  <- 500   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.1
df.heatmapN <- expand.grid(mu = seq(-10,10,paso), sigma = seq(paso,5,paso))

# El test pi necesita que la segunda muestra domine a la primera, por eso la definimos en dos trozos

Potencia <- function(mu,sigma){
  rechazos <- replicate(n_rep,{
    Nest <- runif(n, min = - sqrt(3), max = sqrt(3))
    a <- mu - sqrt(3) * sigma
    b <- mu + sqrt(3) * sigma
    N <- runif(n, min = a, max = b)
    pi.test(Nest,N,pi0,B=B_boot,alternative="less")$p_value<=alpha
  })
  return(mean(rechazos))
}

Potencia <- Vectorize(Potencia)
df.heatmapN$R <-Potencia(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)




# Si en vez de las potencias quiero el valor estimado del indice (gamma/pi)


# Para el gamma

inicio <- Sys.time()
library(tidyverse)

source("gamma.test.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
gamma0_less  <- 0.1
gamma0_greater <- 0.9
alpha   <- 0.05  # Nivel de significación
B_boot  <- 1   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.01
df.heatmapN <- expand.grid(mu = seq(-3,3,paso), sigma = seq(paso,4,paso))


gam <- function(mu,sigma) {
  gamma <- vector(mode = "double",length = 100)
  for (i in 1:100) {
    Nest <- runif(n, min = - sqrt(3), max = sqrt(3))
    a <- mu - sqrt(3) * sigma
    b <- mu + sqrt(3) * sigma
    N <- runif(n, min = a, max = b)
    gamma[i] <- gamma.test(Nest,N,gamma0_less,boot=T,B=B_boot,alternative="less")$gamma
  }
  return(mean(gamma))
}
  
gam <- Vectorize(gam)
df.heatmapN$R <-gam(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)


# Para el pi

inicio <- Sys.time()
library(tidyverse)

source("pi.test.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
pi0  <- 0.1
alpha   <- 0.05  # Nivel de significación
B_boot  <- 500   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.01
df.heatmapN <- expand.grid(mu = seq(-10,10,paso), sigma = seq(paso,5,paso))


pip <- function(mu,sigma){
  Nest <- rnorm(n)
  N <- rnorm(n,mu,sigma)
  pi <- pi.test(Nest,N,pi0,B=B_boot,alternative="less")$pi
  return(pi)
}
pip <- Vectorize(pip)
df.heatmapN$R <-pip(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)








### Calculo analitico de gamma

# Para el gamma

inicio <- Sys.time()
library(tidyverse)

source("gamma_exact_unif.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
gamma0_less  <- 0.1
gamma0_greater <- 0.9
alpha   <- 0.05  # Nivel de significación
B_boot  <- 500   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.01
df.heatmapN <- expand.grid(mu = seq(-3,3,paso), sigma = seq(paso,4,paso))


gam <- function(mu,sigma) { # PARA EL TEORICO
  gamma_exact_unif(0,1,mu,sigma)
}

gam <- Vectorize(gam)
df.heatmapN$R <-gam(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)



### Calculo analitico del pi

# Para el pi

inicio <- Sys.time()
library(tidyverse)

source("pi_exact_unif.R")

set.seed(123)  # Para reproducibilidad

# ------------------------------------------------
# Parámetros principales
# ------------------------------------------------
n       <- 100    # Tamaño de muestra por grupo
n_rep   <- 100    # Número de repeticiones para estimar la potencia en cada punto
gamma0_less  <- 0.1
gamma0_greater <- 0.9
alpha   <- 0.05  # Nivel de significación
B_boot  <- 500   # Número de replicaciones bootstrap que hace gamma.test

paso <- 0.01
df.heatmapN <- expand.grid(mu = seq(-3,3,paso), sigma = seq(paso,4,paso))

gam <- function(mu,sigma) { # PARA EL TEORICO
  pi_exact_unif(mu,sigma)
}

gam <- Vectorize(gam)
df.heatmapN$R <-gam(df.heatmapN$mu,df.heatmapN$sigma)
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = R)) + geom_tile() + 
  #scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradientn(colors = rainbow(10))
# scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

fin <- Sys.time()
tiempo_ejecucion <- fin - inicio
print(tiempo_ejecucion)