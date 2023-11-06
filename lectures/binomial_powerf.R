powerf <- function(n, p) {
  z <- qnorm(0.95)
  p0 <- qnorm(0.95)
  x <- (n * p0 + z * sqrt(n * p0 * (1 - p)) - n*p) / sqrt(n * p * (1 - p))
  f <- 1 - pnorm(x)
  return(f)
}