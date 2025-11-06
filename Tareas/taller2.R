library(tidyr)
library(nortest)
library(lmtest)
library(olsrr)

setwd("G:\\Otros ordenadores\\Mi PC\\Universidad\\8 Semestre\\Econometria\\R-scripts\\Tareas")

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
dwtest(Ozone ~ Solar.R + Wind + Temp,data = df)
