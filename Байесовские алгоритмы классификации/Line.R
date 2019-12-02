lines <- function(cov)
{
  determ <-det(cov)
  
  a <- cov[1,1]
  b <- -cov[1,2]
  c <- -cov[2,1]
  d <- cov[2,2]
  
  m1 <- 0
  m2 <- 0
  
  x <- seq(-4-0.1, 4+0.1, 0.1)
  y <- seq(-4-0.1, 4+0.1, 0.1)
  
  A <- a/determ
  B <- d/determ
  C <- (b+c)/determ
  D <- (-2*m1*a-b*m1-c*m1)/determ
  E <- m1*b-y*m1*c-2*m2*d
  F <- a*m1^2+b*m1*m2+m1*m2*c+m2^2*d
  
func <- function(x, y) {
    1/(2*pi*sqrt(determ))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  z <- outer(x, y, func)
  contour(x, y, z)
}

lines(matrix(c(1,0,0,1),2,2))
