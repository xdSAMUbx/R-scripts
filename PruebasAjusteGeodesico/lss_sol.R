lss_sol <- function(A,W,L){
  ATP <- crossprod(A,W)
  N <- ATP%*%A
  C <- ATP%*%L
  
  X <- solve(N,C)
  V <- L - A%*%X
  
  return (list(X = X, V = V))
}