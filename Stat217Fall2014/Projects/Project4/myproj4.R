diamonds <- read.csv("~/Documents/Stat217Fall2014/Projects/Project4/diamonds.csv")
##You only need to run this first chunk of code once after importing the data##
diamonds <- diamonds[,c(2,3,6)]
diamonds$Color <- as.factor(diamonds$Color)
require(mosaic)
##You may need to install the lattice package##
require(lattice)
options(show.signif.stars = F)


#Look at and describe scatterplots
pairs(diamonds)
xyplot(Price~Carat|Color,data=diamonds)
xyplot(log(Price)~log(Carat)|Color,data=diamonds)

#Why should you consider a log transformation?
fit.first <- lm(Price~Carat*Color, data = diamonds)
plot(fit.first, which = 1, pch = 20)

#Look at the interaction model
fit.Int <- lm(log(Price)~I(log(Carat))*Color, data = diamonds)
summary(fit.Int)
anova(fit.Int)
par(mfrow=c(2,2))
plot(fit.Int)
xyplot(log(Price) ~ log(Carat) | Color, type = c("p","r"),data=diamonds)

fit.int.cellmeans <- lm(log(Price)~Color+I(log(Carat)):Color-1, data = diamonds)
confint(fit.int.cellmeans)


