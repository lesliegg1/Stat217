###point with high leverage but not influential
x1.base <- 1:10
y1.base <- 2*x1.base+3+runif(10, 0, 10)
lm.base <- lm(y1.base~x1.base)

x1 <- c(x1.base, 20)
y1 <- c(y1.base, 45)

lm.1 <- lm(y1~x1)

plot(x1, y1, ylim=c(10, 50), xlim=c(0, 25), pch=16)
abline(lm.1)

plot(lm.1, which=5)

#point with high leverage and large residual
x2 <- c(x1.base, 20)
y2<- c(y1.base, 15)

lm.2 <- lm(y2~x2)

plot(x2, y2, ylim=c(10, 50), xlim=c(0, 25), pch=16)
abline(lm.2)

plot(lm.2, which=5)

#point with low leverage and large residual
x3 <- c(x1.base, 6)
y3<- c(y1.base, 45)

lm.3 <- lm(y3~x3)

plot(x3, y3, ylim=c(10, 50), xlim=c(0, 25), pch=16)
abline(lm.3)
abline(lm.base, lty=5, col="magenta")

plot(lm.3, which=5)
