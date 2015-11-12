#Correlation and Regression Chapter

#
mtfires<- read.csv("http://dl.dropboxusercontent.com/u/77307195/climateR2.csv")

BB<- read.csv("http://dl.dropboxusercontent.com/u/77307195/beersbac.csv")
#The EESEE story "Blood Alcohol Content" describes a study in which 16 student volunteers at The Ohio State University drank a randomly assigned number of cans of beer. Thirty minutes later, a police officer measured their blood alcohol content.
plot(BAC~Beers,data=BB)

x<-seq(from=0,to=20,length.out=20)
y<-(x-mean(x))^2
x<-c(x,5,15)
y<-c(y,220,220)

plot(x,y,col='blue',main="r=0")
require(psych) #install.packages("psych")
pairs.panels(BB,ellipses=F,smooth=F)

cor(BAC~Beers,data=BB)
cor(BB)

mtfires$loghectacres<-log(mtfires$hectacres)
mtfiresR<-mtfires[,-3]
cor(mtfiresR)
summary(mtfiresR)

pairs.panels(mtfiresR,ellipses=F,scale=T,smooth=F)

bozemantemps<- read.csv("http://dl.dropboxusercontent.com/u/77307195/tempMV.csv")

require(alr3)
data(ais)
aisR<-ais[,c("Ht","Hc","Bfat")]
summary(aisR)
pairs.panels(aisR,scale=T,ellipse=F,smooth=F)
cor(aisR)
require(corrplot)
corrplot.mixed(cor(aisR))


  aisR[c(56,166),]
aisR2<-aisR[-c(56,166),] #Removes observations in rows 56 and 166

pairs.panels(aisR2,scale=T,ellipse=F,smooth=F)

corrplot.mixed(cor(aisR2))

aisR2<-ais[-c(56,166),c("Ht","Hc","Bfat","Sex")]
require(car)
scatterplot(Hc~Ht|Sex,data=aisR2,pch=c(3,21),reg.line=F,smoother=F,boxplots="xy",main="Scatterplot of Height vs Hematocrit by Sex")


require(mosaic)
cor(Hc~Ht,data=aisR2[aisR2$Sex==0,]) #Males only
cor(Hc~Ht,data=aisR2[aisR2$Sex==1,]) #Females only

scatterplot(Hc~Bfat|Sex,data=aisR2,pch=c(3,21),reg.line=F,smoother=F,boxplots="xy",main="Scatterplot of Bodyfat vs Hematocrit by Sex")

cor(Hc~Bfat,data=aisR2[aisR2$Sex==0,]) #Males only
cor(Hc~Bfat,data=aisR2[aisR2$Sex==1,]) #Females only

scatterplot(Bfat~Ht|Sex,data=aisR2,pch=c(3,21),reg.line=F,smoother=F,boxplots="xy",main="Scatterplot of Height vs Bodyfat by Sex")

cor(Bfat~Ht,data=aisR2[aisR2$Sex==0,]) #Males only
cor(Bfat~Ht,data=aisR2[aisR2$Sex==1,]) #Females only


#Beers and BAC data set bootstrapping:
BB<- read.csv("http://dl.dropboxusercontent.com/u/77307195/beersbac.csv")
require(mosaic)

Tobs <- cor(BAC~Beers,data=BB); Tobs

par(mfrow=c(1,2))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-cor(BAC~Beers,data=resample(BB))
}
quantiles<-qdata(c(.025,.975),Tstar) #95% Confidence Interval
quantiles

hist(Tstar,labels=T)
abline(v=Tobs,col="red",lwd=3)
abline(v=quantiles$quantile,col="blue",lty=2,lwd=3)

plot(density(Tstar),main="Density curve of Tstar")
abline(v=Tobs,col="red",lwd=3)
abline(v=quantiles$quantile,col="blue",lty=2,lwd=3)




#Regression material:
require(car)
scatterplot(BAC~Beers,data=BB,smooth=F)
require(psych)
pairs.panels(BB,ellipses=F)


#tree example
require(spuRs) #install.packages("spuRs")
data(ufc)
scatterplot(height.m~dbh.cm,data=ufc,smooth=F,reg.line=F)

ufc[168,]

cor(dbh.cm~height.m,data=ufc)
cor(dbh.cm~height.m,data=ufc[-168,])


Tobs <- cor(dbh.cm~height.m,data=ufc); Tobs

par(mfrow=c(2,1))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-cor(dbh.cm~height.m,data=resample(ufc))
}
quantiles<-qdata(c(.025,.975),Tstar) #95% Confidence Interval
quantiles

hist(Tstar,labels=T,main= "Bootstrap distribution of correlation with all data",xlim=c(0.6,0.9))
abline(v=Tobs,col="red",lwd=3)
abline(v=quantiles$quantile,col="blue",lty=2,lwd=3)

Tobs <- cor(dbh.cm~height.m,data=ufc[-168,]); Tobs

Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-cor(dbh.cm~height.m,data=resample(ufc[-168,]))
}
quantiles<-qdata(c(.025,.975),Tstar) #95% Confidence Interval
quantiles

hist(Tstar,labels=T,main= "Bootstrap distribution of correlation without outlier",xlim=c(0.6,0.9))
abline(v=Tobs,col="red",lwd=3)
abline(v=quantiles$quantile,col="blue",lty=2,lwd=3)




#### Regression material####

BB<- read.csv("http://dl.dropboxusercontent.com/u/77307195/beersbac.csv")
#The EESEE story "Blood Alcohol Content" describes a study in which 16 student volunteers at The Ohio State University drank a randomly assigned number of cans of beer. Thirty minutes later, a police officer measured their blood alcohol content.
require(car)
scatterplot(BAC~Beers,ylim=c(0,.2),xlim=c(0,9),data=BB,boxplot=F,main="Scatterplot with regression line",lwd=2,smooth=F)


abline(v=1:9,col="grey")
abline(h=c(0.05914,0.0771),col="blue",lty=2,lwd=2)


m1<-lm(BAC~Beers,data=BB)
summary(m1)



aisR2<-ais[-c(56,166),c("Ht","Hc","Bfat","Sex")]
scatterplot(Hc~Bfat,data=aisR2[aisR2$Sex==1,],smooth=F,main="Scatterplot of BodyFat vs Hematocrit for Female Athletes",ylab="Hc (% blood)",xlab="Body fat (% weight)")
m2=lm(Hc~Bfat,data=aisR2[aisR2$Sex==1,]) #Results for Females 
summary(m2)
confint(m2)

s1=seq(from=0, to=0.03,length.out=100)
ss1=numeric(0)
for (j in (1:length(s1))){
  ss1[j]=sum(lm(BAC~offset(Beers*s1[j]),data=BB)$residuals^2)
}
plot(s1,ss1,type="l",col="blue",lwd=2,xlab="Possible Slope Coefficients",ylab="Sum of squared residuals",main="Sums of squared residuals for different Slopes (Beers vs BAC data)")
abline(v=0.01796,col="red")

require(tigerstats)
require(manipulate)
FindRegLine()

mtfires<- read.csv("http://dl.dropboxusercontent.com/u/77307195/climateR2.csv")
mtfires$loghectacres<-log(mtfires$hectacres)

fire1<-lm(loghectacres~Temperature,data=mtfires)
summary(fire1)
confint(fire1,level=0.99)
qt(.995,df=21)

scatterplot(loghectacres~Temperature,data=mtfires,smoother=T,main="Scatterplot with regression line for Area burned vs Temperature")


#Two data sets with same regression line:

BB<- read.csv("http://dl.dropboxusercontent.com/u/77307195/beersbac.csv")
#The EESEE story "Blood Alcohol Content" describes a study in which 16 student volunteers at The Ohio State University drank a randomly assigned number of cans of beer. Thirty minutes later, a police officer measured their blood alcohol content.
require(car)

m1<-lm(BAC~Beers,data=BB)

par(mfrow=c(1,3))
plot(BAC~Beers,ylim=c(-0.15,.35),xlim=c(1,9),data=BB,main="Real Data")
abline(a=m1$coef[1],b=m1$coef[2],col="green")

BBfake1=BB
BBfake1$BAC=fitted(m1)+residuals(m1)*5
plot(BAC~Beers,ylim=c(-0.15,.35),xlim=c(1,9),data=BBfake1,main="Fake Data")
m1<-lm(BAC~Beers,data=BBfake1)
abline(a=m1$coef[1],b=m1$coef[2],col="green")

BBfake2=BB
BBfake2$BAC=fitted(m1)+residuals(m1)/15
plot(BAC~Beers,ylim=c(-0.15,.35),xlim=c(1,9),data=BBfake2,main="Fake Data")
m1<-lm(BAC~Beers,data=BBfake2)
abline(a=m1$coef[1],b=m1$coef[2],col="green")

#Leverage and Influence explorations
BBfake3=BB
m1<-lm(BAC~Beers,data=BB)
par(mfrow=c(1,2))
id=rep(16,16)
id[3]=21; id[15]=5

par(mfrow=c(1,2))
plot(BAC~Beers,data=BB,pch=id,cex=1.3)
abline(a=m1$coef[1],b=m1$coef[2],col="green")
plot(m1,which=5,add.smooth=F,pch=id,cex=1.3)

BBfake3=BB
BBfake3[17,]=c(11,0.01)
m1o=lm(BAC~Beers,data=BB)
par(mfrow=c(4,2))
id1=rep(16,17)
id1[17]=8;
id2=rep(16,17)
id2[17]=1;


m1=lm(BAC~Beers,data=BBfake3)
summary(m1)
plot(BAC~Beers,data=BBfake3,col=id2,pch=id1,main="Added (11,0.01), R-sq=0.24")
abline(a=m1o$coef[1],b=m1o$coef[2],col="blue",lty=2)
abline(a=m1$coef[1],b=m1$coef[2],col="green")
plot(m1,which=5,add.smooth=F,col=id2,pch=id1,main="Added (11,0.1)")


BBfake4=BB
BBfake4[17,]=c(11,0.19)
m1=lm(BAC~Beers,data=BBfake4)
summary(m1)
plot(BAC~Beers,data=BBfake4,col=id2,pch=id1,main="Added (11,0.19), R-sq=0.86")
abline(a=m1o$coef[1],b=m1o$coef[2],col="blue",lty=2)
abline(a=m1$coef[1],b=m1$coef[2],col="green")
plot(m1,which=5,add.smooth=F,col=id2,pch=id1,main="Added (11,0.19)")


BBfake5=BB
BBfake5[17,]=c(5,0.19)
m1=lm(BAC~Beers,data=BBfake5)
summary(m1)
plot(BAC~Beers,data=BBfake5,col=id2,pch=id1,main="Added (5,0.19), R-sq=0.57")
abline(a=m1o$coef[1],b=m1o$coef[2],col="blue",lty=2)
abline(a=m1$coef[1],b=m1$coef[2],col="green")
plot(m1,which=5,add.smooth=F,col=id2,pch=id1,main="Added (5,0.19)")

BBfake6=BB
BBfake6[17,]=c(5,0.01)
m1=lm(BAC~Beers,data=BBfake6)
summary(m1)
plot(BAC~Beers,data=BBfake6,col=id2,pch=id1,main="Added (5,0.01), R-sq=0.69")
abline(a=m1o$coef[1],b=m1o$coef[2],col="blue",lty=2)
abline(a=m1$coef[1],b=m1$coef[2],col="green")
plot(m1,which=5,add.smooth=F,col=id2,pch=id1,main="Added (5,0.1)")

plot(m1)


m1<-lm(BAC~Beers,data=BB)
par(mfrow=c(2,2))
plot(m1,add.smooth=F,main="Beers vs BAC")

require(spuRs) #install.packages("spuRs")
data(ufc)
require(car)
scatterplot(height.m~dbh.cm,data=ufc[-168,],smooth=F)
tree1<-lm(height.m~dbh.cm,data=ufc[-168,])
summary(tree1)
par(mfrow=c(2,2))
plot(tree1,add.smooth=F)

require(MASS)
data(geyser)
G2<-data.frame(Waiting=geyser$waiting[-1],Duration=geyser$duration[-299])
scatterplot(Waiting~Duration,data=G2,spread=F)
OF1<-lm(Waiting~Duration,data=G2)
summary(OF1)
par(mfrow=c(2,2))
plot(OF1)

#Bozeman temps example
bozemantemps<- read.csv("http://dl.dropboxusercontent.com/u/77307195/tempMV.csv")
require(car)
bozemantemps$Year<-1900:2008
scatterplot(meanmax~Year,data=bozemantemps,ylab="Mean Maximum Temperature (degrees F)",spread=F,main="Scatterplot of Bozeman Yearly Average Max Temperatures")
temp1<-lm(meanmax~Year, data=bozemantemps)
par(mfrow=c(2,2))
plot(temp1,add.smooth=F)

require(effects)
plot(allEffects(temp1),ci.style="lines")

bozemantemps$Year2<-bozemantemps$Year-1900
summary(bozemantemps$Year2)

temp2<-lm(meanmax~Year2,data=bozemantemps)
summary(temp2)
confint(temp2)

require(mosaic)
Tobs <- lm(meanmax~Year,data=bozemantemps)$coef[2]
par(mfrow=c(1,2))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-lm(meanmax~shuffle(Year),data=bozemantemps)$coef[2]
}
hist(Tstar,xlim=c(-1,1)*Tobs)
abline(v=c(-1,1)*Tobs,col="red",lwd=3)
plot(density(Tstar),main="Density curve of Tstar",xlim=c(-1,1)*Tobs)
abline(v=c(-1,1)*Tobs,col="red",lwd=3)


pdata(abs(Tobs),abs(Tstar),lower.tail=F)

bozemantemps$meanmaxC<- (bozemantemps$meanmax-32)*(5/9)
temp3=lm(meanmaxC~Year2, data=bozemantemps)
summary(temp1)
summary(temp3)

mtfires<- read.csv("http://dl.dropboxusercontent.com/u/77307195/climateR2.csv")
mtfires$loghectacres<-log(mtfires$hectacres)

par(mfrow=c(1,2))
plot(hectacres~Temperature,data=mtfires, main="(a)",ylab="Hectacres",pch=16)
plot(loghectacres~Temperature,data=mtfires, main="(b)",ylab="log-Hectacres",pch=16)

require(spuRs) #install.packages("spuRs")
data(ufc)
require(car)
scatterplot(height.m~dbh.cm,data=ufc[-168,],main="Tree height vs tree diameter",smooth=T,spread=F)
scatterplot(log(height.m)~dbh.cm,data=ufc[-168,],smooth=T,spread=F,main="log-Tree height vs tree diameter")
scatterplot(height.m~log(dbh.cm),data=ufc[-168,],smooth=T,spread=F,main="Tree height vs log-tree diameter")
scatterplot(log(height.m)~log(dbh.cm),data=ufc[-168,],smooth=T,spread=F,main="log-Tree height vs log-tree diameter")

require(car)
data(UN)
par(mfrow=c(2,2))


#############CI and PI code################
 BB<- read.csv("http://dl.dropboxusercontent.com/u/77307195/beersbac.csv")
 m1<-lm(BAC~Beers,data=BB)
 beerf<-seq(from=0,to=10,length.out=30)
BBCI<-data.frame(predict(m1,newdata= data.frame(Beers=beerf),interval="confidence"))
head(BBCI)
BBPI<-data.frame(predict(m1,newdata=data.frame(Beers=beerf),interval="prediction"))
head(BBPI)

plot(BAC~Beers,data=BB,xlab="Beers", ylab="BAC",pch=20,col="blue", main="Scatterplot of estimated regression line with 95% CI and PI")
 lines(beerf,BBCI$fit,col="blue",lwd=3)
 lines(beerf,BBCI$lwr,col="red",lty=2,lwd=3)
 lines(beerf,BBCI$upr,col="red",lty=2,lwd=3)
 lines(beerf,BBPI$lwr,col="grey",lty=3,lwd=3)
 lines(beerf,BBPI$upr,col="grey",lty=3,lwd=3)
legend("topleft", c("Estimate", "CI","PI"),lwd=3,lty=c(1,2,3),col = c("blue", "red","grey"))




#################################################################################


#Maybe next time or for quizzes?
require(DMwR)
data(algae)
pairs.panels(algae)

require(mlmRev)
data(Mmmec)
data(star) #Revisit for MLR?



require(aplpack)

bagplot.pairs(trees,col.baghull="green", col.loophull="lightgreen")


require(corrplot)
data(Leaves)
corrplot.mixed(Leaves)

#Later for exploring correlations

require(gpairs)

gpairs(trees,upper.pars=list(scatter="corrgram"))

require(psych)
pairs.panels(trees)

pairs.panels(trees,lm=T,smooth=T,cor=F)

pairs.panels(trees,lm=T,ellipses=F,scale=T)

