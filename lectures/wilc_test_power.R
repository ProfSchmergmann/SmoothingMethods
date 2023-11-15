pi_wilc <- function(m, n, alpha) {
  # Power function of Wilcoxon's signed rank test
  k <- qnorm(1 - alpha) / sqrt(3 * n) + 0.5
  theta <- 1 - pnorm(-sqrt(2) * m)
  eta <- integrate(function(z)f(z, m), lower = -Inf, upper = Inf)$value
  xi <- eta - theta^2
  power <- 1 - pnorm(sqrt(n) * (k - theta) / sqrt(4 * xi))
  return(power)
}

f <- function(z, m) {
  (1 - pnorm(-2 * m - z))^2 * dnorm(z)
}