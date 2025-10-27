source("genA.R")

condition <- function(data,ref = NULL, Weigth = FALSE){
  stopifnot(inherits(data,"data.frame"))
  
  d <- as.numeric(data[,4])
  if (!(isTRUE(Weigth))){
    P <- diag(nrow(data))
  } else {
    P <- diag(1/d)
  }
  
  A <- genA(data, ref = ref )$A
  
  if(!(is.null(ref))){
    mapaReferencias <- setNames(ref$h, ref$nom)
    corr <- apply(data, 1, function(fila) {
      if (fila[["ini"]] %in% names(mapaReferencias)) {
        mapaReferencias[fila[["ini"]]]
      } else if (fila[["fin"]] %in% names(mapaReferencias)) {
        mapaReferencias[ fila[["fin"]] ]
      } else {
        0
      }
    })
    data[,3] <- data[,3] + as.numeric(corr) 
  }
  
  B <- t(Null(A))
  y <- matrix(as.numeric(data[,3]))
  PI <- solve(P)
  M <- B%*%PI%*%t(B)
  W <- B%*%y
  v <- PI%*%t(B)%*%solve(M)%*%W
  Y <- y - v
  return(list(B = B, P = P, W = W, M = M, v = v, Y = Y))
}