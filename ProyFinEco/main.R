# RScript trabajo final Econometria
library("dplyr")
library("tidyr")
library("ggplot2")
library("xtable")
library("stargazer")

# Rutas de acceso
setwd("D:/Universidad/8 Semestre/Econometria/R-scripts/ProyFinEco")

# Rutas a la data
data <- read.csv("data/database.csv", header = T, sep = ",")
dfLoc <- as.data.frame(table(data$LOCALIDAD))
colnames(dfLoc) <- c("Localidades", "Num. Datos")
print(xtable(dfLoc), include.rownames = FALSE) # Datos por localidad

# Manejo de la database
df <- data %>%
  select(-c(LOCALIDAD, ID, LATITUD, LONGITUD, LINK)) %>%
  mutate(across(everything(), ~ as.numeric(gsub(",", "", .))))

df <- setNames(df, c("xPrec", "xHues", "xHPriv", "xBed", "xBath", "xPet", "xCalGen", "xLimp",
  "xRes", "xProp", "xLav", "xEntre", "xInter", "xUten", "xEsta"))

#########################
# Analisis Exploratorio #
#########################

# Estadísticas descriptivas de la información
statsDf <- round(as.data.frame(t(sapply(df, function(col) {
  c(min = min(col), med = mean(col), max = max(col), var = var(col), desv = sd(col))
}))), 3)
print(xtable(statsDf), include.ronames = FALSE)

# Densidad por 3 variables
x11()
plot(density(df$xPrec, na.rm = TRUE), main = "Densidad del Precio")
plot(density(df$xCalGen, na.rm = TRUE), main = "Densidad de Calificación General")
plot(density(df$xRes, na.rm = TRUE), main = "Densidad de Número de Reseñas")

# Box plots de variables utilizadas
boxplot(df$xPrec, names = "Precio", col = "skyblue", main = "BoxPlot Precio")
boxplot(df$xCalGen, names = "Calificación General", col = "red", main = "BoxPlot Calificación General")
boxplot(df$xRes, names = "Número de Reseñas", col = "green", main = "BoxPlot Número de Reseñas")

############################
# Modelos lineales Simples #
############################

summary(modHues <- lm(xPrec ~ xHues, data = df))
summary(modHPriv <- lm(xPrec ~ xHPriv, data = df))
summary(modBed <- lm(xPrec ~ xBed, data = df))
summary(modBath <- lm(xPrec ~ xBath, data = df))
summary(modPet <- lm(xPrec ~ xPet, data = df))
summary(modCalGen <- lm(xPrec ~ xCalGen, data = df))
summary(modLimp <- lm(xPrec ~ xLimp, data = df))
summary(modRes <- lm(xPrec ~ xRes, data = df))
modelos1 <- list(modHues, modHPriv, modBed, modBath)
modelos2 <- list(modCalGen, modLimp, modRes)
stargazer(modelos1, type = "text")
stargazer(modelos2, type = "latex")
# Supuesto 1 - Residuos
resid <- lapply(modelos, function(m) {
  r <- mean(residuals(m))
  r[abs(r) < 1e-10] <- 0
  return (r)
})
resid
