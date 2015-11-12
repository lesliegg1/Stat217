#Gundale, Bach, Nordin (2013)
#Biomass data:

gdn<-read.csv("http://dl.dropboxusercontent.com/u/77307195/gundalebachnordin_2.csv")
m1<-lm(Massperha~Species*Treatment,data=gdn)
summary(m1)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(m1,sub.caption="Initial Massperha 2-WAY model")

require(mosaic)
require(beanplot)
beanplot(Massperha~Species+Treatment,data=gdn, side = "b", col = list("white","lightgreen"),xlab="Treatment",ylab="Biomass")
legend("topright", bty="n",c("HS", "PS"), fill = c("white","lightgreen"))

#For cover
gdn$T=factor(as.numeric(gdn$Treatment))
beanplot(Massperha~Species+T,data=gdn, side = "b", col = list("white","lightgreen"),horizontal=T)
legend("topright", bty="n",c("A", "B"), fill = c("white","lightgreen"))

source("http://dl.dropboxusercontent.com/u/77307195/intplot.R")
intplot(Massperha~Species*Treatment,data=gdn,col=c(1,2),lwd=2)

summary(gdn$Massperha)

#Transform:
gdn$logMassperha<-log(gdn$Massperha)
par(mfrow=c(1,2))
beanplot(logMassperha~Species+Treatment,data=gdn, side = "b", col = list("white","lightgreen"),xlab="Treatment",ylab="log-Biomass",main="(a)")
legend("topright", bty="n",c("HS", "PS"), fill = c("white","lightgreen"))
intplot(logMassperha~Species*Treatment,data=gdn,col=c(1,2),lwd=2,main="(b)")


m2=lm(logMassperha~Species*Treatment,data=gdn)
summary(m2)
require(car)
Anova(m2)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(m2,sub.caption="log-Massperha 2-WAY model")

require(effects)
plot(allEffects(m2),multiline=T,ci.style="bars")


#Flatten and do one-way analysis:

#Create new variable:
gdn$SpTrt<-interaction(gdn$Species,gdn$Treatment)
levels(gdn$SpTrt)

newm2=lm(logMassperha~SpTrt,data=gdn)
require(car)
Anova(newm2)
require(multcomp)
PWnewm2 <- glht(newm2, linfct = mcp(SpTrt = "Tukey"))
 confint(PWnewm2)
old.par <- par(mai=c(1.5,3,1,1)) #Makes room on the plot for the group names
plot(PWnewm2)
cld(PWnewm2)

intplot(logMassperha~Species*Treatment,cld=T,cldshift=.15,cldcol=c(2,3,4,5,6,8),data=gdn,lwd=2,main="Interaction with CLD from Tukey's HSD on One-Way ANOVA")



require(gplots)
plotmeans(logMassperha~SpTrt,main="Plot of mean log-Biomass by groups",data=gdn,ylab="log-Biomass", mean.labels = T,digits=2 ,barcol="red",col="black")
text(c(1),c(7.7),"bd",col="blue",cex=2)
text(c(2),c(8.1),"d",col="blue",cex=2)
text(c(3),c(7.6),"b",col="blue",cex=2)
text(c(4),c(8.1),"cd",col="blue",cex=2)
text(c(5),c(6.6),"a",col="blue",cex=2)
text(c(6),c(7.6),"bc",col="blue",cex=2)

##############################
#Sasaki and Pratt (2013) - ANT Data
##############################

sasakipratt<-read.csv("http://dl.dropboxusercontent.com/u/77307195/sasakipratt.csv")

sasakipratt$group<-factor(sasakipratt$group)
levels(sasakipratt$group)<-c("Light","Entrance")

sasakipratt$after<-factor(sasakipratt$after)
levels(sasakipratt$after)<-c("SmallBright","LargeDark")

sasakipratt$before<-factor(sasakipratt$before)
levels(sasakipratt$before)<-c("SmallBright","LargeDark")

plot(after~group,data=sasakipratt)

require(mosaic)
tally(~group+after,data=sasakipratt)

table1<-tally(~group+after,data=sasakipratt,margins=F)
mosaicplot(table1,main="Mosaicplot of Ant Colony Data")

chisq.test(table1,correct=F)

chisq.test(table1,correct=F)$expected

chisq.test(table1,correct=F)$residuals


#############################
#Benson and Mannion (2012)
#MLR data on biodiversity:
##############################
bm<-read.csv("http://dl.dropboxusercontent.com/u/77307195/bensonmanion.csv")
bm2<-bm[,-c(9:10)]
require(psych)
pairs.panels(bm2,ellipses=F)
bm$logSpecies<-log(bm$Species)
bm$logDBCs<-log(bm$DBCs)
bm$logDBFs<-log(bm$DBFs)
bm$TJK<-factor(bm$TJK)

scatterplot(logSpecies~StageNumber,data=bm,spread=F)
require(car)

scatterplot(logSpecies~logDBCs|TJK,data=bm,smooth=F,main="Scatterplot of log-diversity vs log-DBCs by period",lwd=2)

scatterplot(log(Species)~log(DBFs),data=bm,smooth=F)

bd1<-lm(logSpecies~logDBCs+TJK,data=bm)
bd2<-lm(logSpecies~logDBFs+TJK,data=bm)

require(MuMIn)
options(na.action = "na.fail")
dredge(bd1, rank="AIC",extra = c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))
dredge(bd2, rank="AIC",extra = c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))

par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(bd1)

bm2$TJK=factor(bm2$TJK)
bd1=lm(logSpecies~logDBCs+TJK,data=bm2)

summary(bd1)
confint(bd1)

plot(allEffects(bd1))


#Replicating results:

require(nlme)
bd1=gls(logSpecies~logDBCs+TJK,data=bm2,correlation=corAR1(),method="ML")

dredge(bd1)

#Doing it right?
require(mgcv)

gam1=gam(Species~offset(logDBCs)+s(StageNumber,bs="ts",k=15),data=bm2,family="poisson")
plot(gam1)
summary(gam1)
plot(gam1,shift=-2.94588,trans=exp)

#Or just as a GLM?

glm1=glm(Species~offset(logDBCs)+StageNumber,data=bm2,family="poisson")
summary(glm1)

