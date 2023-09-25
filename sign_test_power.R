# Power functions of normal test and sign test

pi_norm <- function(mu, n, alpha) {
  z <- qnorm(p = 1 - alpha)
  f <- 1 - pnorm(q = z - (mu / (1 / sqrt(n))))
  return(f)
}

pi_sign <- function(m, n, alpha) {
  # Power function of sign test at the Normal
  k <- qbinom(1 - alpha, size = n, prob = 1 / 2)
  f <- 1 - pbinom(k, size = n, prob = 1 - pnorm(-m))
  return(f)
}