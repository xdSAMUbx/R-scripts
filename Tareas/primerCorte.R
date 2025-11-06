#######################
# Carpeta de trabajo #
######################
setwd("D:/Universidad/8 Semestre/Econometria/scripts/Tareas") # PC
setwd("D:/programacion/R-scripts/Tareas") # Portatil
#############################
# Funciones Complementarias #
#############################
source("funciones.R")
##############
# Capítulo 2 #
##############

# Vectores
x <- c(1:5)
# Matrices
x <- as.matrix(matrix(x,5,1,TRUE))
# Transpuesta 
tx <- t(x)
# Suma matricial
x <- as.matrix(c(1,2)); y <- as.matrix(c(3,4))
z = x + y
# Producto matricial
z = x%*%t(y)
# Matrices Particionadas
A <- as.matrix(matrix(c(1:9),3,3,TRUE))
B <- as.matrix(matrix(c(1:9),3,3,TRUE))

P <-cbind(A,B) # Añade a la derecha
P <-rbind(A,B) # Añade debajo
P <- cbind(rbind(A,B), rbind(B,A)) # Matriz como las de Geoestadística (Compuesta)

# Matrices diagonales
diag(rep(1,3)) # Permite repetir elementos x cantidad de veces

# Ejercicios

# 1
a <- as.matrix(c(1:3)); b <- as.matrix(c(4:6)); u <- as.matrix(c(3:1))
v <- as.matrix(c(6:4)); w <- as.matrix(matrix(c(7:9),1,3,TRUE))

a + b
v - a 
t(w) + b 
3*u
t(w) - a
v/3
a%*%t(b)
b%*%t(a)

# 2
x = as.matrix(c(2,2,-3)); y = as.matrix(c(1,-2,1))
vecP1 <- list(a,b,u,v)
for(i in seq_along(vecP1))
{
  if(is.ort(x,vecP1[[i]]))
    {
      print(i)
      break
    }
}
for(i in seq_along(vecP1))
{
  if(is.ort(y,vecP1[[i]]))
  {
    print(i)
    break
  }
}

# 3
A = as.matrix(matrix(c(1:6),2,3,T)); B = as.matrix(matrix(c(1:6),3,2,T))
U = as.matrix(matrix(c(1:4),2,2,T)); V = as.matrix(matrix(c(5:8),2,2,T))
W = as.matrix(matrix(c(2,2,3,5),2,2,T)); Z = as.matrix(matrix(c(3,2,3,6),2,2,T))

#3.a
A%*%B
t(A%*%B)
B%*%A
# Error t(a)%*%A
# Error t(a)%*%(A%*%a)
V%*%diag(U)
diag(t(B%*%A))
U%*%V%*%W%*%Z
diag(diag(U%*%V))
diag(diag(U))%*%diag(diag(V))

##############
# Capítulo 3 #
##############
