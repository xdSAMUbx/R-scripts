setwd("D:/Universidad/8 Semestre/Econometria/scripts/Tareas")

######################
# Funciones Externas #
######################
source("funciones.R")
#############################
#     Ejercicios Cap 2      #
#############################

# Matrices fila y columnas en minusculas
a <- as.matrix(c(1:3)); b <- as.matrix(c(4:6))

# Matrices en mayusculas
A <- as.matrix(matrix(c(1:25),5,5,TRUE))
B <- as.matrix(matrix(c(26:50),5,5,TRUE))

# Operaciones básicas de matrices
c <- a + b
c <- a - b

# Multiplicación de matrices
c <- a%*%t(b)

# Funciones
suma <- function(x,y){return (x+y)}
suma(1,2)

prodPoint(a,b)


#Ejercicio 1 - Capitulo 2
a <- as.matrix(c(1:3)); b <- as.matrix(c(4:6)); u <- as.matrix(c(3:1)); 
v <- as.matrix(c(6:4)); w <- as.matrix(matrix(c(7:9),1,3,TRUE))

a + b
v - a 
t(w) + b
3*u
t(w) - a
v/3
a%*%t(b)
b%*%t(a)

# Ejercicio 2 
x = as.matrix(c(2,2,-3)); y = as.matrix(c(1,-2,1))
