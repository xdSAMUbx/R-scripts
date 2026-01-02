rlss_sol <- function(A,W,K,K0,L){
  
  ATP <- crossprod(A,W)
  N <- ATP%*%A
  c <- ATP%*%L
  m <- nrow(N)
  l <- nrow(K)
  N_R <- Matrix(0,nrow = m + l, ncol = m + l, sparse = TRUE)
  C_R <- Matrix(0,nrow = m + l, ncol = 1, sparse = TRUE)
  
  N_R[1:m, 1:m] <- N
  N_R[1:m, (m+1):(m+l)] <- t(K)
  N_R[(m+1):(m+l), 1:m] <- K
  
  C_R[1:m,1] <- c
  C_R[(m+1):(m+l), 1] <- K0
  
  X <- Matrix(solve(N_R,C_R)[1:m,],dimnames = list(colnames(A)))
  V <- L - A%*%X
  return (list(X = X, V = V))
}