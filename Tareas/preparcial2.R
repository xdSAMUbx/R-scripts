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

bicis <- data.frame(
  Brand_Model = c(
    "Klein Räve v","Giant OCR Composite 3","Giant OCR 1","Specialized Roubaix",
    "Trek Pilot 2.1","Cannondale Synapse 4","LeMond Poprad","Raleigh Cadent 1.0",
    "Giant RCR3","Schwinn Super Sport GS","Fuji Absolute 2.0","Jamis Coda Comp",
    "Cannondale Road Warrior 400","Schwinn Sierra GS","Mongoose Switchback SX",
    "Giant Sedona DX","Jamis Explorer 4.0","Diamondback Wildwood Deluxe",
    "Specialized Crossroads Sport"
  ),
  Type = c(
    "Road","Road","Road","Road","Road","Road","Road","Road",
    "Fitness","Fitness","Fitness","Fitness","Fitness",
    "Comfort","Comfort","Comfort","Comfort","Comfort","Comfort"
  ),
  Weight = c(
    20,22,22,21,21,21,22,24,
    23,23,24,26,25,
    31,32,32,35,34,31
  ),
  Price = c(
    1800,1800,1000,1300,1320,1050,1350,650,
    630,700,700,830,700,
    340,280,360,600,350,330
  )
)

plot(bicis$Weight, bicis$Price, pch=21,bg="black",col="red", cex=1,
     main="Peso vs Precio", xlab = "Peso", ylab="Precio")
grid()

bicis$Road <- ifelse(bicis$Type == "Road", 1,0)
bicis$Fitness <- ifelse(bicis$Type == "Fitness", 1,0) 
bicis$Comfort <- ifelse(bicis$Type == "Comfort", 1,0)

bicis$Pesos2 <- bicis$Weight^2

summary(mod1 <- lm(Price ~ Weight + Pesos2, data = bicis))
summary(mod2 <- lm(Price ~ Weight + Road + Comfort, data = bicis))
summary(mod3 <- lm(Price ~ Weight + Road + Comfort + Weight:Road + Weight:Comfort, 
                   data = bicis))

shapiro.test(residuals(mod3))
bptest(mod3)
vif(mod3)
durbinWatsonTest(mod3,10)

summary(mod4 <- lm(Price ~ Road + Weight:Road,data = bicis))
shapiro.test(residuals(mod4))
bptest(mod4)
vif(mod4)
durbinWatsonTest(mod4,10)
acf(residuals(mod4))
