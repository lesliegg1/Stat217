#MLR Chapter code
snotel_s<-read.csv("http://dl.dropboxusercontent.com/u/77307195/snotel_s.csv")
snotel2<-snotel_s[,c(1:2,4:6,3)] #Reorders columns for nicer pairs.panel display
require(psych)
pairs.panels(snotel2[,-c(1:2)],ellipse=F,main="Scatterplot matrix of SNOTEL Data")

m1<-lm(Snow.Depth~Elevation,data=snotel2)
m2<-lm(Snow.Depth~Min.Temp,data=snotel2)
m3<-lm(Snow.Depth~Max.Temp,data=snotel2)
require(effects)
plot(allEffects(m1),main="SLR: Effect of Elevation",ci.style="lines")
plot(allEffects(m2),main="SLR: Effect of Min Temp",ci.style="lines")
plot(allEffects(m3),main="SLR: Effect of Max Temp",ci.style="lines")

summary(m1)
summary(m2)
summary(m3)

m4<-lm(Snow.Depth~Elevation+Min.Temp+Max.Temp,data=snotel2)
summary(m4)
plot(allEffects(m4),main="MLR model with Elev, Min and Max Temps",ci.style="lines")

par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(m4,sub.caption="Diagnostics for m4")

m5<-lm(Snow.Depth~Elevation+Min.Temp+Max.Temp,data=snotel2[-9,])
summary(m5)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(m5,sub.caption="Diagnostics for m5")

plot(allEffects(m5),main="MLR model with NE Entrance Removed",ci.style="lines")

m6<-lm(Snow.Depth~Elevation+Min.Temp+Max.Temp,data=snotel2[-c(9,22),])
summary(m6)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(m6,sub.caption="Diagnostics for m6")

plot(allEffects(m6),main="MLR model with n=23",ci.style="lines")

mean(snotel2[-c(9,22),]$Min.Temp)
mean(snotel2[-c(9,22),]$Max.Temp)


#Making own effect plot:
elevs<-c(5000,6000,8000)
snowdepths<-c(-6.19,20.71,74.51)
plot(snowdepths~elevs,ylim=c(-20,100),cex=2,main="Effect plot of elevation by hand",col="blue",pch=16)
lines(snowdepths~elevs,col="red",lwd=2)

elevs<-seq(from=5000,to=8000,length.out=30)
newdata1=data.frame(Elevation=elevs,Min.Temp=rep(27.826,30),Max.Temp=rep(36.3913,30))
newdata1

predict(m6,newdata=newdata1,interval="confidence")

m6<-lm(Snow.Depth~Elevation+Min.Temp+Max.Temp,data=snotel2[-c(9,22),])
summary(m6)

m1<-lm(Snow.Depth~Elevation,data=snotel2[-c(9,22),])
summary(m1)
m2<-lm(Snow.Depth~Min.Temp,data=snotel2[-c(9,22),])
summary(m2)

m3<-lm(Snow.Depth~Max.Temp,data=snotel2[-c(9,22),])
summary(m3)
m4<-lm(Snow.Depth~Elevation+Min.Temp,data=snotel2[-c(9,22),])
summary(m4)
m5<-lm(Snow.Depth~Elevation+Max.Temp,data=snotel2[-c(9,22),])
summary(m5)
m7<-lm(Snow.Depth~Min.Temp+Max.Temp,data=snotel2[-c(9,22),])
summary(m7)

plot(Max.Temp~Elevation,data=snotel2[-c(9,22),],pch=16,main="Plot of Max Temp and Elevation")

require(corrplot)
par(mfrow=c(1,1),oma=c(0,0,1,0))
corrplot.mixed(cor(snotel2[-c(9,22),3:6]))

round(cor(snotel2[-c(9,22),3:6]),2)


#VIFs:

require(car)
vif(m6)
sqrt(vif(m6))

#VIF calc:
elev1<-lm(Elevation~Min.Temp+Max.Temp,data=snotel2[-c(9,22),])
summary(elev1)

#Finding CIs
confint(m5)


#MLR Case study:

require(openintro)
data(satGPA)
require(psych)
pairs.panels(satGPA,ellipse=F)

gpa1<-lm(FYGPA~SATV+SATM,data=satGPA)
summary(gpa1)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(gpa1,sub.caption="Diagnostics for GPA model with SATV and SATM")

require(car)
vif(gpa1)
sqrt(vif(gpa1))

require(effects)
plot(allEffects(gpa1))
confint(gpa1)


require(car)
scatterplot(FYGPA~SATM|sex,data=satGPA)


predict(gpa1,newdata=data.frame(SATV=30,SATM=60))
predict(gpa1,newdata=data.frame(SATV=30,SATM=60),interval="confidence")
predict(gpa1,newdata=data.frame(SATV=30,SATM=60),interval="prediction")

#Remake effects plots with 95% PIs
dv1<-data.frame(SATV=seq(from=24,to=76,length.out=50),SATM=rep(54.4,50))
dm1<-data.frame(SATV=rep(48.93,50),SATM=seq(from=29,to=77,length.out=50))

mv1<-data.frame(predict(gpa1,newdata=dv1,interval="confidence"))
pv1<-data.frame(predict(gpa1,newdata=dv1,interval="prediction"))

mm1<-data.frame(predict(gpa1,newdata=dm1,interval="confidence"))
pm1<-data.frame(predict(gpa1,newdata=dm1,interval="prediction"))

par(mfrow=c(1,2))

plot(dv1$SATV,mv1$fit,lwd=2,ylim=c(pv1$lwr[1],pv1$upr[50]),type="l",xlab="SATV Percentile",ylab="GPA",main="SATV Effect, CI and PI")
lines(dv1$SATV,mv1$lwr,col="red",lty=2,lwd=2)
lines(dv1$SATV,mv1$upr,col="red",lty=2,lwd=2)
lines(dv1$SATV,pv1$lwr,col="grey",lty=3,lwd=3)
lines(dv1$SATV,pv1$upr,col="grey",lty=3,lwd=3)
legend("topleft", c("Estimate", "CI","PI"),lwd=3,lty=c(1,2,3),col = c("black", "red","grey"))

plot(dm1$SATM,mm1$fit,lwd=2,ylim=c(pm1$lwr[1],pm1$upr[50]),type="l",xlab="SATM Percentile",ylab="GPA",main="SATM Effect, CI and PI")
lines(dm1$SATM,mm1$lwr,col="red",lty=2,lwd=2)
lines(dm1$SATM,mm1$upr,col="red",lty=2,lwd=2)
lines(dm1$SATM,pm1$lwr,col="grey",lty=3,lwd=3)
lines(dm1$SATM,pm1$upr,col="grey",lty=3,lwd=3)

require(car)
scatterplot(FYGPA~SATV|sex,lwd=3,data=satGPA,spread=F,smooth=F,main="Scatterplot of GPA vs SATV by Sex")

scatterplot(FYGPA~SATM|sex,lwd=3,data=satGPA,spread=F,smooth=F,main="Scatterplot of GPA vs SATM by Sex")

#m1=lm(FYGPA~SATM*sex,data=satGPA)
#anova(m1)
satGPA$SEX<-factor(satGPA$sex)
levels(satGPA$SEX)<-c("MALE","FEMALE")
satGPA$SEXINDICATOR<-as.numeric(satGPA$SEX=="FEMALE")

head(data.frame(SEX=satGPA$SEX,SEXINDICATOR=satGPA$SEXINDICATOR),10)

SATSex1<-lm(FYGPA~SATM+SEX,data=satGPA)
summary(SATSex1)

SATSex2<-lm(FYGPA~SATM+SEXINDICATOR,data=satGPA)
summary(SATSex2)


model.matrix(SATSex1)[1:10,]

fitmales<-0.216+0.0386*dm1$SATM
fitfemales<-0.529+0.0386*dm1$SATM

plot(dm1$SATM,fitmales,ylim=c(min(fitmales),max(fitfemales)),main="Plot of estimated regression lines by sex",type="l",lwd=2,xlab="SATM",ylab="FYGPA")
lines(dm1$SATM,fitfemales,lwd=3,col="red")
abline(v=50,lty=2,col="grey")
abline(h=0.216+0.0386*50,lty=2,col="black")
abline(h=0.529+0.0386*50,lty=2,col="red")
legend("topleft", c("Male", "Female"),lwd=c(2,3),lty=1,col = c("black", "red"))


require(effects)
plot(allEffects(SATSex1))

confint(SATSex1)

#Four group example:
require(heplots)
data(Headache)
scatterplot(du2~du1|treatment,data=Headache,smooth=F,lwd=2,main="Plot of Maximum DB tolerances before and after treatment (by treatment)")
Headache$treatment<-factor(Headache$treatment)
head1<-lm(du2~du1+treatment,data=Headache)
dummies<-model.matrix(head1)[,c(3:5)]
colnames(dummies)<-c("I_T2", "I_T3","I_Control")
res1<-data.frame(Treatment=Headache$treatment,dummies)[1:17,]
names(res1)[2]<-"Intercept"

du1s<-seq(from=0.37,to=15.1,length.out=50)
hdT1<-data.frame(du1=du1s,treatment=rep("T1",50))
hdT2<-data.frame(du1=du1s,treatment=rep("T2",50))
hdT3<-data.frame(du1=du1s,treatment=rep("T3",50))
hdcontrol<-data.frame(du1=du1s,treatment=rep("Control",50))

plot(du1s,predict(head1,newdata=hdT1),lwd=2,lty=2,ylim=c(0,16),type="l",xlab="DU1",ylab="DU2",main="Plot of additive estimated regression model")
lines(du1s,predict(head1,newdata=hdT2),lwd=2,col="red")
lines(du1s,predict(head1,newdata=hdT3),lwd=2,col="green",lty=2)
lines(du1s,predict(head1,newdata=hdcontrol),lwd=2,col="blue",lty=3)
legend("topleft",lwd=2,title="Groups",legend=c("T1","T2","T3","Control"),lty=c(2,1,2,3),col=c("black","red","green","blue"))

summary(head1)
require(effects)
plot(allEffects(head1))

par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(head1,sub.caption="Plot of diagnostics for additive model with du1 and treatment for du2")
require(car)
vif(head1)

head1R<-lm(du2~du1,data=Headache)
summary(head1R)

anova(head1)


#REturn to headache data with interaction model:

require(heplots)
data(Headache)
Headache$treatment<-factor(Headache$treatment)
head2<-lm(du2~du1*treatment,data=Headache)
summary(head2)

plot(allEffects(head2),x.var="du1",multiline=T,ci.style="bands")


require(smdata)
 data("dyslexic3")
 ?dyslexic3
 scatterplot(score~ziq|dys,xlab="Standardized nonverbal IQ scores",ylab="Reading score",data=dyslexic3,smooth=F,main="Plot of IQ vs Reading by dyslexia status")
dyslexic3$dys<-factor(dyslexic3$dys) #Because dys was just numerically coded - makes it a factor
dys_model<-lm(score~ziq*dys,data=dyslexic3)
summary(dys_model)

par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(dys_model,sub.caption="Plot of diagnostics for Dyslexia Interaction model")

require(effects)
plot(allEffects(dys_model),ci.style="bands",multiline=T)

dys_modelR<-lm(score~ziq+dys,data=dyslexic3)
summary(dys_modelR)
plot(allEffects(dys_modelR))

dys_modelR2<-lm(score~dys,data=dyslexic3)
summary(dys_modelR2)
plot(allEffects(dys_modelR2))


require(car)
vif(dys1)


#ANOVA testing examples:

require(car)
Anova(head2)

head1<-lm(du2~du1+treatment,data=Headache)
Anova(head1)

require(coneproj)
data(FEV)
FEV$sex<-factor(FEV$sex)
levels(FEV$sex)<-c("Female","Male")
FEV$smoke<-factor(FEV$smoke)
levels(FEV$smoke)<-c("Nonsmoker","Smoker")
require(car)
scatterplot(log(FEV)~age|smoke,data=FEV,smooth=F,main="Plot of log(FEV) vs Age of children by smoking status")
fm1<-lm(log(FEV)~height+age*smoke+sex,data=FEV)
summary(fm1)
par(mfrow=c(2,2),oma=c(0,0,2,0))
plot(fm1,sub.caption="Diagnostics for full FEV model")
fm1R<-lm(log(FEV)~height+age+smoke+sex,data=FEV)


AIC(fm1,fm1R)

require(MuMIn)
options(na.action = "na.fail") #Must run this code once to use dredge
dredge(fm1, rank="AIC",extra = c("R^2",adjRsq=function(x) summary(x)$adj.r.squared))


################Other data not used but considered#################

data(census)


data(gifted)
histPlot(gifted$count)
histPlot(gifted$fatheriq)
histPlot(gifted$motheriq)
histPlot(gifted$motheriq - gifted$fatheriq)
plot(gifted$score ~ gifted$motheriq)
lm(gifted$score ~ gifted$motheriq + gifted$fatheriq + gifted$speak +
     gifted$count + gifted$read +
     gifted$edutv + gifted$cartoons)