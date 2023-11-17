gcv_mcycle <- function(K){
  knots <- seq(a,b,length.out=K+2)[2:(K+1)]
  Phi <- bs(x,knots=knots,degree=3,intercept=T,Boundary.knots = c(a,b))
  H <- Phi %*% solve(t(Phi)%*%Phi) %*% t(Phi)
  yhat <- H%*%y
  df <- sum(diag(H))
  n <- length(y)
  gcv <- mean((y-yhat)^2)/(1-df/n)^2
  return(gcv)
}