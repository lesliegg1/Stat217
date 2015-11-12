
#Examine Data
data(chickwts)
head(chickwts)
require(mosaic)
favstats(weight~feed, data=chickwts)
boxplot(weight~feed, data=chickwts,ylab="Weight in Grams")
chickwts$feed<-factor(chickwts$feed,levels=c("horsebean","casein","linseed", "meatmeal","soybean","sunflower"))

#ANOVA
lm1<-lm(weight~feed,data=chickwts)
anova(lm1)
summary(lm1)

# Diagnostic Plots
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))

#Multiple Comparison
require(multcomp)
pairs<-glht(lm1, linfct=mcp(feed="Tukey"))
confint(pairs)
old.par <- par(mai=c(1.5,2.5,1,1))
plot(pairs)