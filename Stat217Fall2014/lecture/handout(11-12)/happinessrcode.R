##########
# Multicollinearity/causation issue ..

set.seed(113)
caff <- rnorm(20,mean=200,sd=10)
o.caff <- sort(caff)
happiness <- -100 + 2*o.caff + rnorm(20,mean=0,sd=10)

income <- 15000 + 5*o.caff + rnorm(20,0,sd=10)
plot(o.caff,income)

options(show.signif.stars=FALSE)

c.lm <- lm(happiness ~ o.caff)
summary(c.lm)
AIC(c.lm)
anova(c.lm)

i.lm <- lm(happiness ~ income)
summary(i.lm)
AIC(i.lm)
anova(i.lm)

ic.lm <- lm(happiness ~ income + o.caff)
summary(ic.lm)
AIC(ic.lm)
anova(ic.lm)

ic.lm2 <- lm(happiness ~ o.caff + income)
summary(ic.lm2)
anova(ic.lm2)


pairs(~happiness + income + o.caff)

cor(income,o.caff)


beta.1.hat <- coef(ic.lm2)[2]
beta.1.hat.times.caff <- beta.1.hat*o.caff
par.resids.caff <- beta.1.hat.times.caff + ic.lm2$resid  #component + residuals
plot(happiness, par.resids.caff)

plot(o.caff,happiness)
abline(caff.lm)

par(mfrow=c(1,2))
plot(o.caff, happiness)
abline(c.lm)
text(190, 340, "Slope = 1.8")
plot(o.caff, par.resids.caff) 
abline(0, coef(ic.lm)[3])
text(210, -310, "Slope = -1.7")
