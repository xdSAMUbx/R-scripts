gen_matrix_weight <- function(data, weight = FALSE, sparse = TRUE){
  library(Matrix)
  
  # Revision
  stopifnot(inherits(data[,4],"numeric"))
  
  n <- nrow(data)

  if (any(data[,4] <= 0, na.rm=TRUE)) {
    stop("Las distancias deben ser positivas y distintas de cero")
  }
  
  if (isTRUE(weight)){
    diagonal <- 1/data[,4]
  } else {
    diagonal <- rep(1,n)
  }
  
  if (isTRUE(sparse)){
    w <- Matrix::Diagonal(x = diagonal)
  } else {
    w <- diag(diagonal)
  }
  
  return (w = w)
}