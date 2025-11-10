library(MASS)
library(tidyr)
library(nortest)
library(lmtest)
library(olsrr)
library(car)
library(caret)
library(psych)
library(skedastic)
library(rms)
library(nlme)

setwd("G:\\Otros ordenadores\\Mi PC\\Universidad\\8 Semestre\\Econometria\\R-scripts\\Tareas")


#############################
#         Primer Punto      #
#############################
data("airquality")

df <- as.data.frame(airquality)

# 1) Gráfico x vs y
x11(width=10,height=4)

op <- par(mfrow = c(1, 3))

panel <- function(x, y, titulo, xlab, ylab, pbg, lcol){
  plot(x, y, type="n", main=titulo, xlab=xlab, ylab=ylab)
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col="lightgray", border=NA)
  grid(col="white", lwd=2)
  points(x, y, pch=21, bg=pbg, col="black", cex=1.5)
  box(); axis(1); axis(2)
}

panel(df$Solar.R, df$Ozone,
      "Radiación Solar vs Ozono",
      "Radiación Solar (Ly)", "Ozono (ppb)",
      "purple", "darkred")

panel(df$Wind, df$Ozone,
      "Velocidad del Viento vs Ozono",
      "Vel. del Viento (mph)", "Ozono (ppb)",
      "lightblue", "blue")

panel(df$Temp, df$Ozone,
      "Temperatura vs Ozono",
      "Temperatura (°F)", "Ozono (ppb)",
      "orange", "red")

par(op)

# 2) Modelo RLM
summary(mod1 <- lm(Ozone ~ Solar.R + Wind + Temp, data = df, na.action = na.omit))
resmod1 <- residuals(mod1)
sum(resmod1)
# Normalidad
shapiro.test(resmod1)
lillie.test(resmod1)
# Heterocedasticidad
bptest(Ozone ~ Solar.R + Wind + Temp, data = df)
# Autocorrelación
dwtest(Ozone ~ Solar.R + Wind + Temp,data = df,alternative = "two.sided")
durbinWatsonTest(mod1,10)
acfmod1 <- acf(resmod1, main="Autocorrelación Residuales", ylab="Autocorrelación", xlab="Rezago")
acfmod1
# Multicolinealidad
mod1vif <- vif(mod1)

# Transformación boxcox
bc <- MASS::boxcox(Ozone ~ Solar.R + Wind + Temp, data=df, lambda = seq(-2,2,length=100))
L <- bc$x[which.max(bc$y)]

df$OzT1 <- ifelse(df$Ozone == 0,ln(df$Ozone),
       df$Ozone^(L-1)/(L * geometric.mean(df$Ozone)^(L-1)))

df$OzT2 <- ifelse(df$Ozone == 0,ln(df$Ozone),
                  (df$Ozone^L-1)/L)

df$OzT3 <- ifelse(df$Ozone == 0,ln(df$Ozone),
                  df$Ozone^L)

summary(mod1T1 <- lm(OzT1~Solar.R + Wind + Temp,data = df))
shapiro.test(residuals(mod1T1))
bptest(mod1T1)
acf(residuals(mod1T1))
durbinWatsonTest(mod1T1,10)

summary(mod1T2 <- lm(OzT2~Solar.R + Wind + Temp,data = df,na.action = na.omit))
# Normalidad
shapiro.test(residuals(mod1T2))
lillie.test(residuals(mod1T2))
# Heterocedasticidad
bptest(mod1T2)
# Autocorrelación
acf(residuals(mod1T2), main = "Autocorrelación en los Residuos",
    ylab = "Autocorrelación", xlab  = "Rezagos")
dwtest(mod1T2,alternative="two.sided")
durbinWatsonTest(mod1T2,10)
# Multicolinealidad
vif(mod1T2)

summary(mod1T3 <- lm(OzT3~Solar.R + Wind + Temp,data = df))
shapiro.test(residuals(mod1T3))
bptest(mod1T3)
dwtest(mod1T3)
acf(residuals(mod1T3))
durbinWatsonTest(mod1T3,10)
plot(density(residuals(mod1T3)))

summary(mod2T2 <- lm(OzT2~Solar.R + Wind + Temp+Month + Day, data = df,na.action = na.omit))
# Normalidad
shapiro.test(residuals(mod2T2))
lillie.test(residuals(mod2T2))
# Heterocedasticidad
bptest(mod2T2)
# Autocorrelación
dwtest(mod2T2)
durbinWatsonTest(mod2T2,10)
fin <- acf(residuals(mod2T2),main="Autocorrelación de los Residuos", xlab = "Rezagos",
    ylab = "Autocorrelación")
# Multicolinealidad
vif(mod2T2)

############################
#       Segundo Punto      #
############################

data2 <- "G:\\Otros ordenadores\\Mi PC\\Universidad\\8 Semestre\\Econometria\\Taller2\\data7-24.csv"
df2 <- read.csv(data2)
# a
summary(mod2 <- lm(salepric ~ sqft + bedrms + baths + garage + age + city, data = df2))

# b
plot(residuals(mod2), main = "Residuos vs Orden Observación", ylab = "Residuos",
     xlab = "Observaciones", pch = 21, cex = 1, col="black", bg = "red")
abline(h = 0, lwd=2)
grid()

plot(df2$sqft,residuals(mod2), main = "Residuos vs Área (sqft)", ylab = "Residuos",
     xlab = "Área (sqft)", pch=21, cex=1, col="black", bg = "darkblue")
abline(h=0, lwd=2, col="green")
grid()

# c
bptest(mod2)
white(mod2)

# D
df2$e2 <- residuals(mod2)^2
summary(mod22 <- lm(e2 ~ sqft, data = df2))
df2$sigma2_est <- pmax(fitted(mod22),1e-6)
w <-  1/df2$sigma2_est
w_2 <- 1/df2$sqft^2
summary(mod23 <- lm(salepric ~ sqft + bedrms + baths + garage + age + city, 
                    data = df2, weights = w))

# E
summary(mod24 <- gls(salepric ~ sqft + bedrms + baths + garage + age + city, 
    data = df2, weights = varFixed(~ w_2)))
bptest(mod24)
