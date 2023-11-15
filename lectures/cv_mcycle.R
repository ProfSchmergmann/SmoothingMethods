cv_mcycle <- function(k) {
  knots <- seq(a, b, length.out = k + 2)[2:(k + 1)]
  Phi <- bs(x, knots = knots, degree = 3, intercept = T, Boundary.knots = c(a, b))
  H <- Phi %*% solve(t(Phi) %*% Phi) %*% t(Phi)
  y_hat <- H %*% y
  df <- sum(diag(H))
  n <- length(y)
  gcv <- mean((y - y_hat)^2 / (1 - df / n)^2)
  return(gcv)
}