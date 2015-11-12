require(heplots)
require(mosaic)
xyplot(Years~Attr, data=MockJury, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,4.33, 1.2, 4.33, lwd=5)
  panel.segments(1.8, 3.97, 2.2, 3.97, lwd=5)
  panel.segments(2.8, 5.81, 3.2, 5.81, lwd=5) })

library(MASS)
require(lattice)
sd.1 <- c(mvrnorm(n = 10, rep(5,1), 1, tol = 1e-6, empirical = TRUE), 
       mvrnorm(n = 10, rep(10,1), 1, tol = 1e-6, empirical = TRUE), 
       mvrnorm(n = 10, rep(15,1), 1, tol = 1e-6, empirical = TRUE))
mean(sd.1)
sd(sd.1)
x<-c(rep(1,10),rep(2,10),rep(3,10))
x<-ifelse(sd.1<=7, "Group1", ifelse(sd.1>=8 & sd.1<=12, "Group2", "Group3"))
z <- as.factor(x)

par(mfrow=c(2,2))

one <- xyplot(sd.1~z, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,5, 1.2, 5, lwd=5)
  panel.segments(1.8, 10, 2.2, 10, lwd=5)
  panel.segments(2.8, 15, 3.2, 15, lwd=5) })

sd.9 <- c(mvrnorm(n = 10, rep(5,1), 9, tol = 1e-6, empirical = TRUE), 
       mvrnorm(n = 10, rep(10,1), 9, tol = 1e-6, empirical = TRUE), 
       mvrnorm(n = 10, rep(15,1), 9, tol = 1e-6, empirical = TRUE))
nine <- xyplot(sd.9~z, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,5, 1.2, 5, lwd=5)
  panel.segments(1.8, 10, 2.2, 10, lwd=5)
  panel.segments(2.8, 15, 3.2, 15, lwd=5) })

sd.16 <- c(mvrnorm(n = 10, rep(5,1), 16, tol = 1e-6, empirical = TRUE), 
          mvrnorm(n = 10, rep(10,1), 16, tol = 1e-6, empirical = TRUE), 
          mvrnorm(n = 10, rep(15,1), 16, tol = 1e-6, empirical = TRUE))
sixteen <- xyplot(sd.16~z, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,5, 1.2, 5, lwd=5)
  panel.segments(1.8, 10, 2.2, 10, lwd=5)
  panel.segments(2.8, 15, 3.2, 15, lwd=5) })

sd.100 <- c(mvrnorm(n = 10, rep(5,1), 100, tol = 1e-6, empirical = TRUE), 
           mvrnorm(n = 10, rep(10,1), 100, tol = 1e-6, empirical = TRUE), 
           mvrnorm(n = 10, rep(15,1), 100, tol = 1e-6, empirical = TRUE))
hundred <- xyplot(sd.100~z, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,5, 1.2, 5, lwd=5)
  panel.segments(1.8, 10, 2.2, 10, lwd=5)
  panel.segments(2.8, 15, 3.2, 15, lwd=5) })

sd.500 <- c(mvrnorm(n = 10, rep(5,1), 500, tol = 1e-6, empirical = TRUE), 
            mvrnorm(n = 10, rep(10,1), 500, tol = 1e-6, empirical = TRUE), 
            mvrnorm(n = 10, rep(15,1), 500, tol = 1e-6, empirical = TRUE))
fivehundred <- xyplot(sd.500~z, panel = function(x, y) {
  panel.xyplot(x, y)
  panel.segments(0.8,5, 1.2, 5, lwd=5)
  panel.segments(1.8, 10, 2.2, 10, lwd=5)
  panel.segments(2.8, 15, 3.2, 15, lwd=5) })

print(one, split=c(1,1,2,2), more=TRUE)
print(nine, split=c(2,1,2,2), more=TRUE)
#print(sixteen, split=c(2,1,3,1), more=TRUE)
print(hundred, split=c(1,2,2,2), more=TRUE)
print(fivehundred, split=c(2,2,2,2))

lm.1 <- lm(sd.1~z)
lm.9 <- lm(sd.9~z)
lm.100 <- lm(sd.100~z)
lm.500 <- lm(sd.500~z)

anova(lm.1);
anova(lm.9);
anova(lm.100);
anova(lm.500)

fivehundred.wo <- xyplot(sd.500~z)
print(fivehundred.wo)

one.wo <-  xyplot(sd.1~z)
print(one.wo)
