# Taller 1 Econometria
setwd("D:/Universidad/8 Semestre/Econometria/R-scripts/Tareas")
library("wooldridge")
data(beveridge)
View(beveridge)
urate <- beveridge$urate
vrate <- beveridge$vrate
df <- data.frame(urate, vrate)

# PUNTO 1 - A
x <- matrix(c(rep(1, nrow(beveridge)), beveridge$vrate), nrow = nrow(beveridge), ncol = 2)
y <- matrix(c(beveridge$urate))
betas <- solve(crossprod(x, x)) %*% crossprod(x, y)
summary(mod1a <- lm(urate ~ 1 + vrate, data = df))
mean(residuals(mod1a))

# PUNTO 1 - B
summary(mod2a <- lm(log10(urate) ~ 1 + log10(vrate)))
mean(residuals(mod2a))

# PUNTO 1 - C
summary(mod3a <- lm(urate ~ 1 + I(1 / vrate)))
mean(residuals(mod3a))

# PUNTO 1 - D
new <- data.frame(vrate = 10.5)
pred1 <- predict(object = lm(log10(urate) ~ 1 + log10(vrate)), new, interval = "predict", level = 0.98)
