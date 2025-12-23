# Vamos a pillar los valores mas representativos de la potencia

library(tidyverse)
library(scales)
library(xtable)
library(reshape2)

graficar <- function(df.heatmapN) {
ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = Potencia)) + geom_tile() + 
  scale_fill_gradientn(
    colors = c("#006600", "white", "#FF0000"),
    values = rescale(c(0, 0.05, 1)),
    limits = c(0, 1)
  ) +
  #scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = alpha) +
  # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
}

load("Pi_pot_greater.Rdata")
df.heatmapN$Potencia <- df.heatmapN$R
graficar(df.heatmapN)
df.heatmapN$mu <- round(df.heatmapN$mu,1)
df.heatmapN$sigma <- round(df.heatmapN$sigma,1)
Tabla <- filter(df.heatmapN,mu %in% c(-10 ,-5,0,5,10), sigma %in% c(5, 4, 3, 2, 1))
Tabla <- Tabla[,-3]

tabla_potencia <- dcast(
  Tabla,                # tu data-frame
  factor(sigma, levels = rev(c(1,2,3,4,5))) ~ mu,                 # filas = mu, columnas = sigma
  value.var      = "Potencia",
  fun.aggregate  = mean,      # qué hacer si hay duplicados
  fill           = 0          # valor para los cruces ausentes
)

tabla_xtable <- xtable(tabla_potencia)
align(tabla_xtable) <- c("|c", "c|", "|c|","|c|","|c|","|c|","|c|")
print(tabla_xtable, type = "latex", file = "tabla.tex", include.rownames = FALSE, digits = c(0,0,2))




### Para las tablas de los indices

library(tidyverse)
library(scales)
library(xtable)
library(reshape2)

graficar <- function(df.heatmapN) {
  ggplot(df.heatmapN, aes(x = mu, y = sigma, fill = Indice)) + geom_tile() + 
    scale_fill_gradientn(
      colors = rainbow(10)) +
    #scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = alpha) +
    # scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
    labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
}

load("Pi_indice_estimado.RData")
df.heatmapN$Indice <- df.heatmapN$R
graficar(df.heatmapN)
df.heatmapN$mu <- round(df.heatmapN$mu,1)
df.heatmapN$sigma <- round(df.heatmapN$sigma,1)
Tabla <- filter(df.heatmapN,mu %in% c(-3 ,-1.5,0,1.5,3), sigma %in% c(4, 3, 2, 1))
Tabla <- Tabla[,-3]

tabla_potencia <- dcast(
  Tabla,                # tu data-frame
  factor(sigma, levels = rev(c(1,2,3,4))) ~ mu,                 # filas = mu, columnas = sigma
  value.var      = "Indice",
  fun.aggregate  = mean,      # qué hacer si hay duplicados
  fill           = 0          # valor para los cruces ausentes
)

tabla_xtable <- xtable(tabla_potencia)
align(tabla_xtable) <- c("|c", "c|", "|c|","|c|","|c|","|c|","|c|")
print(tabla_xtable, type = "latex", file = "tabla.tex", include.rownames = FALSE, digits = c(0,0,2))