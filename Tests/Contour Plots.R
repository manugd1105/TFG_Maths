library(tidyverse)

paso <- 0.01
df.heatmapN <- expand.grid(mu = seq(-5,5,paso), lambda = seq(paso,4,paso))
#Normal
RGauss <- function(mu,lambda){
  if(lambda==1&mu<=0){1
    } else if(lambda==1&mu>0){0
    } else if(lambda<1){1-punif(pnorm(mu/(1-lambda)),0,1) #El punif() se puede quitar
    } else if(lambda>1){punif(pnorm(mu/(1-lambda)),0,1) } #El punif() se puede quitar
}
RGauss <- Vectorize(RGauss)
df.heatmapN$R <-RGauss(df.heatmapN$mu,df.heatmapN$lambda)
ggplot(df.heatmapN, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
#  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
#Dando menos peso a las colas
RCentr <- function(mu,lambda){
  if(lambda==1&mu<=0){1
  } else if(lambda==1&mu>0){0
  } else if(lambda<1){1-pbeta(pnorm(mu/(1-lambda)),10,10)
  } else if(lambda>1){pbeta(pnorm(mu/(1-lambda)),10,10) }
}
RCentr <- Vectorize(RCentr)
df.heatmapN$R <-RCentr(df.heatmapN$mu,df.heatmapN$lambda)
ggplot(df.heatmapN, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
#  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
#Dando más peso a las colas
RColas <- function(mu,lambda){
  if(lambda==1&mu<=0){1
  } else if(lambda==1&mu>0){0
  } else if(lambda<1){1-pbeta(pnorm(mu/(1-lambda)),0.1,0.1)
  } else if(lambda>1){pbeta(pnorm(mu/(1-lambda)),0.1,0.1) }
}
RColas <- Vectorize(RColas)
df.heatmapN$R <-RColas(df.heatmapN$mu,df.heatmapN$lambda)
ggplot(df.heatmapN, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
#  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
#Dando más peso a la cola izquierda
RIzq <- function(mu,lambda){
  if(lambda==1&mu<=0){1
  } else if(lambda==1&mu>0){0
  } else if(lambda<1){1-pbeta(pnorm(mu/(1-lambda)),1,10)
  } else if(lambda>1){pbeta(pnorm(mu/(1-lambda)),1,10) }
}
RIzq <- Vectorize(RIzq)
df.heatmapN$R <-RIzq(df.heatmapN$mu,df.heatmapN$lambda)
ggplot(df.heatmapN, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
#  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
#Dando más peso a la cola derecha
RDer <- function(mu,lambda){
  if(lambda==1&mu<=0){1
  } else if(lambda==1&mu>0){0
  } else if(lambda<1){1-pbeta(pnorm(mu/(1-lambda)),10,1)
  } else if(lambda>1){pbeta(pnorm(mu/(1-lambda)),10,1) }
}
RDer <- Vectorize(RDer)
df.heatmapN$R <-RDer(df.heatmapN$mu,df.heatmapN$lambda)
ggplot(df.heatmapN, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
#  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5) +
  scale_fill_gradient2(low = "#666666", high = "black", mid = "white", midpoint = 0.5) +
  labs(x="\u03BC", y="\u03C3") + theme(axis.title.y = element_text(angle = 0, vjust = 0.5))






#Uniforme parametrizada en función de mu y lambda, partiendo de U(0,1)
paso <- 0.01
df.heatmapU <- expand.grid(mu = seq(-3,3,paso), lambda = seq(paso,4,paso))
Runif <- function(mu,lambda, aX=0, bX=1){
  aY <- mu
  bY <- mu + lambda
  if(aX<aY&bX<bY){0
  } else if(aX<aY&bX>=bY){(bY-bX)/(aX-bX-aY+bY)
  } else if(aX>=aY&bX<bY){(aX-aY)/(aX-bX-aY+bY)
  } else if(aX>=aY&bX>=bY){1}
}
Runif <- Vectorize(Runif)
df.heatmapU$R <- Runif(df.heatmapU$mu,df.heatmapU$lambda)
ggplot(df.heatmapU, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5)







#Uniforme parametrizada en función de mu y lambda, partiendo de U(-0.5,0.5)
paso <- 0.01
df.heatmapU2 <- expand.grid(mu = seq(-3,3,paso), lambda = seq(paso,4,paso))
Runif2 <- function(mu,lambda, aX=-0.5, bX=0.5){
  aY <- mu-lambda/2
  bY <- mu+lambda/2
  if(aX<aY&bX<bY){0
  } else if(aX<aY&bX>=bY){(bY-bX)/(aX-bX-aY+bY)
  } else if(aX>=aY&bX<bY){(aX-aY)/(aX-bX-aY+bY)
  } else if(aX>=aY&bX>=bY){1}
}
Runif2 <- Vectorize(Runif2)
df.heatmapU2$R <- Runif2(df.heatmapU2$mu,df.heatmapU2$lambda)
ggplot(df.heatmapU2, aes(x = mu, y = lambda, fill = R)) + geom_tile() + 
  scale_fill_gradient2(low = "#FF0000", high = "#006600", mid = "white", midpoint = 0.5)