pi_exact_unif <- function(mu, sigma) {
  SQ3 <- sqrt(3)
  # soporte de X e Y
  leftX  <- -SQ3; rightX  <-  SQ3
  leftY  <-  mu - SQ3*sigma
  rightY <-  mu + SQ3*sigma
  
  # 1) sin solapamiento puro
  if (leftY >= rightX) return(0)
  if (rightY <= leftX) return(1)
  
  # 2) región común
  lo <- max(leftX, leftY)
  hi <- min(rightX, rightY)
  
  # 3) función diferencia D(x) = F_Y(x)-F_X(x)
  D <- function(x) {
    FY <- if      (x < leftY)         0
    else if (x > rightY)        1
    else (x - leftY)/(rightY-leftY)
    FX <- if      (x < leftX)         0
    else if (x > rightX)        1
    else (x - leftX)/(rightX-leftX)
    FY - FX
  }
  
  # 4) supremo en los extremos + 0
  max(0, D(lo), D(hi))
}
