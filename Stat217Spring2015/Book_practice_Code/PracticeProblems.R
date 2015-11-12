# The following code provides the basic code required for completing the R portions of the practice problems:

#############################
#########Chapter 0###########

treadmill<-read.csv("http://dl.dropboxusercontent.com/u/77307195/treadmill.csv",header=T)
favstats(treadmill$Age)
favstats(treadmill$BodyWeight)
hist(treadmill$Age)
boxplot(treadmill$Age)
treadmill$BWlb<-2.205*treadmill$BodyWeight
par(mfrow=c(2,2)) #Code to allow multiple graphs to be placed on a single graph in different panels 
hist(treadmill$BodyWeight,main="Body Weight Histogram (kgs)")
hist(treadmill$BWlb,,main="Body Weight Histogram (lbs)")
boxplot(treadmill$BodyWeight,main="Body Weight Boxplot (kgs)")
boxplot(treadmill$BWlb,main="Body Weight Boxplot (kgs)")

#############################
#########Chapter 1###########

require(mosaicData)
#load the dataset
data(HELPrct)

HELPrct2<-HELPrct[,c("daysanysub","sex")]
HELPrct3<-na.omit(HELPrct2) #Removes any subjects that had missing responses on these two variables

favstats(daysanysub~sex, data = HELPrct2)
favstats(daysanysub~sex, data = HELPrct3)

boxplot(daysanysub~sex, data = HELPrct3)
require(beanplot)
beanplot(daysanysub~sex, data = HELPrct3,log="",method="jitter",col=7)

#Permutation
Tobs<-compareMean(daysanysub~sex, data = HELPrct3)
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-compareMean(daysanysub~shuffle(sex), data = HELPrct3)
}
hist(Tstar,labels=T)
abline(v=Tobs,lwd=2,col="red")
plot(density(Tstar),main="Density curve of Tstar")
abline(v=Tobs,lwd=2,col="red")
pdata(abs(Tobs),abs(Tstar),lower.tail=F)

#Parametric
t.test(daysanysub~sex,data=HELPrct3,var.equal=T)

#Bootstrap
Tobs<-compareMean(daysanysub~sex, data = HELPrct3)
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-compareMean(daysanysub~sex, data = resample(HELPrct3))
}
quantiles<-qdata(c(.025,.975),Tstar)
quantiles
hist(Tstar,labels=T)
abline(v=quantiles$quantile,col="blue",lwd=3)
plot(density(Tstar),main="Density curve of Tstar")
abline(v=quantiles$quantile,col="blue",lwd=3)


#############################
#########Chapter 2###########

require(multcomp)
data(cholesterol)
help(cholesterol)

favstats(response~trt,data=cholesterol)

boxplot(response~trt,data=cholesterol)
require(beanplot)
beanplot(response~trt,data=cholesterol,log="",method="jitter",col=6)

model1<-lm(response~trt,data=cholesterol)
par(mfrow=c(2,2))
plot(model1)
anova(model1)

#Permutation test
Tobs <- anova(lm(response~trt,data=cholesterol))[1,4]; Tobs

par(mfrow=c(1,2))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-anova(lm(response~shuffle(trt),data=cholesterol))[1,4]
}
hist(Tstar,labels=T)
abline(v=Tobs,col="red",lwd=3)
plot(density(Tstar),main="Density curve of Tstar")
abline(v=Tobs,col="red",lwd=3)
pdata(Tobs,Tstar,lower.tail=F)

#Tukey's HSD
par(mfrow=c(1,1))
Tm2 <- glht(model1, linfct = mcp(trt = "Tukey"))
confint(Tm2)
old.par <- par(mai=c(1.5,2,1,1)) #Makes room on the plot for the group names
plot(Tm2)
cld(Tm2)

#############################
#########Chapter 3###########

require(Sleuth2)
require(mosaic)
math <- ex1320
names(math)
favstats(Score~Sex+Background, data = math)

beanplot(Score~Sex+Background, data=math, side = "b", col = list("lightblue", "white"),method="jitter",log="")
legend("topright", bty="n",c("Female", "Male"), fill = c("lightblue", "white"))

source("http://dl.dropboxusercontent.com/u/77307195/intplot.R")
par(mfrow=c(1,2))
intplot(Score~Sex+Background, data=math)
intplot(Score~Background+Sex, data=math)

fit.Math <- lm(Score~Background*Sex, data = math)
requir(car)
Anova(fit.Math)

fit.Math2 <- lm(Score~Background+Sex, data = math)
Anova(fit.Math2)

summary(fit.Math2)
par(mfrow=c(2,2)); plot(fit.Math2)

require(effects)
plot(allEffects(fit.Math2))

#############################
#########Chapter 4###########

csd<- read.csv("http://dl.dropboxusercontent.com/u/77307195/csd.csv")
require(tabplot)
tableplot(csd[,c("isare","careabout", "thoughtabout","Gender","Age","Household.Income","Education")])
d1<-tally(~careabout+isare,data=csd,margins=F)
plot(isare~careabout,data=csd)
mosaicplot(d1)
chisq.test(d1)$expected
chisq.test(d1)
chisq.test(d1)$residuals
mosaicplot(d1,shade=T,main="Data is/are by care about it")


#############################
#########Chapter 5###########

treadmill<-read.csv("http://dl.dropboxusercontent.com/u/77307195/treadmill.csv",header=T)
require(psych)
pairs.panels(treadmill,ellipses=F,smooth=F)
plot(TreadMillOx~RunTime,data=treadmill)
tm1<-lm(TreadMillOx~RunTime,data=treadmill)
summary(tm1)
par(mfrow=c(2,2))
plot(tm1)


#############################
#########Chapter 6###########
summary(tm1)
confint(tm1)
predict(tm1,newdata=data.frame(RunTime=11),interval="confidence")
predict(tm1,newdata=data.frame(RunTime=11),interval="prediction")

predict(tm1,newdata=data.frame(RunTime=16),interval="confidence")
predict(tm1,newdata=data.frame(RunTime=16),interval="prediction")


tm2<-lm(log(TreadMillOx)~RunTime,data=treadmill)
par(mfrow=c(2,2))
plot(tm2)
summary(tm2)


#############################
#########Chapter 7###########

treadmill<-read.csv("http://dl.dropboxusercontent.com/u/77307195/treadmill.csv",header=T)
tm1<-lm(TreadMillOx~RunTime,data=treadmill)

mlr1<-lm(TreadMillOx~RunTime+RunPulse+RestPulse+BodyWeight+Age,data=treadmill)
summary(mlr1)
plot(mlr1)
 require(car)
 vif(mlr1)
mlr2<-lm(TreadMillOx~RunTime+RunPulse+BodyWeight+Age,data=treadmill)
summary(mlr2)

require(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
dredge(mlr1, rank="AIC",extra = c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))

favstats(treadmill$Age)

plot(allEffects(mlr1))

#Create Age bins:
treadmill$Ageb<-cut(treadmill$Age,breaks=c(37,44.5,50.5,58))
summary(treadmill$Ageb)
require(car)
scatterplot(TreadMillOx~RunTime|Ageb,data=treadmill,smooth=F,lwd=2)
mlr_int<-lm(TreadMillOx~RunTime*Ageb,data=treadmill)
summary(mlr_int)
Anova(mlr_int)

mlr_add<-lm(TreadMillOx~RunTime+Ageb,data=treadmill)
summary(mlr_add)
Anova(mlr_int)