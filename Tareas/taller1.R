# Taller 1 Econometria
setwd("D:/Universidad/8 Semestre/Econometria/R-scripts/Tareas")
library("wooldridge")
data(beveridge)
data(crime2)
View(crime2)

###########
# PUNTO 1 #
###########

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
summary(mod2a <- lm(log(urate) ~ 1 + log(vrate)))
mean(residuals(mod2a))

# PUNTO 1 - C
summary(mod3a <- lm(urate ~ 1 + I(1 / vrate)))
mean(residuals(mod3a))

# PUNTO 1 - D
new <- data.frame(vrate = 10.5)
pred1 <- predict(object = lm(urate ~ 1 + I(1 / vrate)), new, interval = "predict", level = 0.98)

###########
# PUNTO 2 #
###########

summary(mod1pt2 <- lm(lcrimes ~ unem + loffic + lpcinc + west + nrtheast + south + larea + pop, data = crime2))
summary(mod2pt2 <- lm(lcrimes ~ officers + popden + crmrte + offarea + area, data = crime2))
summary(mod3pt2 <- lm(lcrimes ~ loffic + west + south + larea, data = crime2))

pred <- data.frame(
  loffic = 6,
  west = 1,
  south = 1,
  larea = 7.68
)

predict(mod3pt2, newdata = pred, interval = "prediction", level = 0.98)

###########
# PUNTO 3 #
###########

dx <- c(37, 38, 18, 50, 22, 55, 42, 29, 63, 13, 60, 62, 36)
px <- c(7, 6, 10, 4, 9, 2, 8, 8, 2, 12, 3, 3, 6)
pz <- c(5, 7, 3, 9, 3, 12, 5, 5, 18, 2, 9, 10, 5)
pw <- c(7, 5, 13, 4, 11, 3, 8, 9, 3, 15, 5, 5, 6)
i <- c(6, 8, 3, 18, 3, 21, 2, 19, 20, 6, 12, 5, 26)

df2 <- data.frame(dx, px, pz, pw, i)
X <- cbind(rep(1, 13), px, pz, pw, i)
betas <- solve(crossprod(X, X)) %*% crossprod(X, dx)
summary(mod1pt3 <- lm(dx ~ px + pz + pw + i, data = df2))
summary(mod2pt3 <- lm(log(dx) ~ log(px) + log(pz) + log(pw) + log(i), data = df2))

# Elasticidades
medDx <- mean(dx)
medPx <- mean(px)
medPz <- mean(pz)
medPw <- mean(pw)
medI <- mean(i)
e1 <- betas[[2]] * (medPx / medDx)
e2 <- betas[[3]] * (medPz / medDx)
e3 <- betas[[4]] * (medPw / medDx)
e4 <- betas[[5]] * (medI / medDx)
