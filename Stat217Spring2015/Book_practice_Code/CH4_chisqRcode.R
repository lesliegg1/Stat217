#Chapter 4 R code:

require(vcd)
data(Arthritis) #Double-blind clinical trial with treatment and control groups
#Homogeneity examplereq
require(tabplot)

tableplot(Arthritis,select=c(Treatment,Improved,Sex,Age))
#require(extracat)
#cpcp(Arthritis)
require(mosaic)
tally(~Treatment+Improved,data=Arthritis)
tally(Improved~Treatment,data=Arthritis)

plot(Improved~Treatment,data=Arthritis,main="Stacked Bar Chart of Arthritis Data")

Arthtable <- tally(~Treatment+Improved,data=Arthritis,margins=F)
Arthtable

chisq.test(Arthtable)$expected
(chisq.test(Arthtable)$residuals)^2

chisq.test(Arthtable)$residuals
mosaicplot(Arthtable,shade=T)

#relative to null model:
ArthritisFAKE <- rbind(Arthritis,Arthritis) # Just to make the following plot!
ArthritisFAKE$Treat<-factor(c(rep("Placebo",84),rep("Treated",84))) #Just to make the following plot
tally(Improved~Treat,data=ArthritisFAKE)
plot(Improved~Treat,data=ArthritisFAKE,main="Homogeneity Null Hypothesis True")

#Permute the group labels once:

Arthperm<-Arthritis
Arthperm$PermTreatment<-factor(shuffle(Arthperm$Treatment))
plot(Improved~PermTreatment,data=Arthperm,main="Stacked Bar Chart of Permuted Arthritis Data")
Arthpermtable<-tally(~PermTreatment+Improved,data=Arthperm,margins=F)
Arthpermtable
chisq.test(Arthpermtable)

#Permutation test:

Tobs <- chisq.test(Arthtable)$statistic; Tobs

par(mfrow=c(1,2))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-chisq.test(tally(~shuffle(Treatment)+Improved,data=Arthritis,margins=F))$statistic
}
hist(Tstar,xlim=c(0,Tobs+1))
abline(v=Tobs,col="red",lwd=3)
plot(density(Tstar),main="Density curve of Tstar",xlim=c(0,Tobs+1),lwd=2)
abline(v=Tobs,col="red",lwd=3)


pdata(Tobs,Tstar,lower.tail=F)






Xsq=seq(from=0,to=15,length.out=30)
plot(Xsq,dchisq(xsq,df=2),ylab="Density",main="Plot of Chi-sq(2) distribution",type="l",col="blue",lwd=2)
abline(v=13.055,col="magenta")

pchisq(13.055,df=2,lower.tail=F)


#Independence example 1

require(poLCA)
data(election) #2000 Survey 
election2<-na.omit(election[,c("PARTY","VOTE3")]) #Subset variables and remove missing values
election2$VOTEF<-factor(election2$VOTE3)
levels(election2$VOTEF)
levels(election2$VOTEF)<-c("Gore","Bush","Other")
levels(election2$VOTEF)
electable<-tally(~PARTY+VOTEF,data=election2,margins=F)
#Put "predictor" first as ~x+y with margins=F to only get counts for analysis
electable
mosaicplot(electable) #Makes a mosaic plot where areas are related to the proportion of the total in the table

chisq.test(electable)$expected

chisq.test(electable)

mosaicplot(electable,shade=T) #Adds information on the size of the residuals
mosaicplot(VOTEF~PARTY,data=election,shade=T)
chisq.test(electable)$residuals #(Obs-expected)/sqrt(expected)
mosaicplot(chisq.test(electable)$expected)


mosaicplot(chisq.test(electable)$expected,main="Plot of H0: Independence True")

plot(INTELG~factor(PARTY),data=election)

Xsq=seq(from=0,to=100,length.out=100)
plot(Xsq,dchisq(Xsq,df=12),ylab="Density",main="Plot of Chi-sq(12) distribution",type="l",col="blue",lwd=2)

pchisq(763.55,df=12,lower.tail=F)

Tobs <- chisq.test(electable)$statistic; Tobs

par(mfrow=c(1,2))
B<- 1000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-chisq.test(tally(~shuffle(PARTY)+VOTEF,data=election2,margins=F))$statistic
}
hist(Tstar)
abline(v=Tobs,col="red",lwd=3)
plot(density(Tstar),main="Density curve of Tstar",lwd=2)
abline(v=Tobs,col="red",lwd=3)


pdata(Tobs,Tstar,lower.tail=F)



data(cheating) #Survey of students
#Create new response variable based on combinations of lieing to avoid exam or paper
#
require(tabplot)
cheating$LIEEXAM<-factor(cheating$LIEEXAM)
cheating$LIEPAPER<-factor(cheating$LIEPAPER)
cheating$FRAUD<-factor(cheating$FRAUD)
cheating$COPYEXAM<-factor(cheating$COPYEXAM)
cheating$GPA<-factor(cheating$GPA)

tableplot(cheating,sort=GPA)


cheating$liar<-interaction(cheating$LIEEXAM,cheating$LIEPAPER)
levels(cheating$liar)<-c("None","ExamLie","PaperLie","LieBoth")

cheating$copier<-interaction(cheating$FRAUD,cheating$COPYEXAM)
levels(cheating$copier)<-c("None","PaperCheat","ExamCheat","PaperExamCheat")

cheatlietable<-tally(~liar+copier,data=cheating,margins=F)
cheatlietable

tableplot(cheating,sort=liar,select=c(liar,copier))

mosaicplot(cheatlietable)

#Collapse the middle categories of each variable
cheating$liar2<-cheating$liar
levels(cheating$liar2)<-c("None","ExamorPaper","ExamorPaper","LieBoth")
cheating$copier2<-cheating$copier
levels(cheating$copier2)<-c("None","ExamorPaper","ExamorPaper","CopyBoth")

tableplot(cheating,sort=liar2,select=c(liar2,copier2))
cheatlietable<-tally(~liar2+copier2,data=cheating,margins=F)
cheatlietable

chisq.test(cheatlietable)$expected

chisq.test(cheatlietable)

pchisq(13.2384,df=4,lower.tail=F)

Xsq=seq(from=0,to=30,length.out=100)
plot(Xsq,dchisq(Xsq,df=4),ylab="Density",main="Plot of Chi-sq(4) distribution",type="l",col="blue",lwd=2)
abline(v=13.24,col="magenta")


#Permutations to find distribution under H0

Tobs<-chisq.test(tally(~liar2+copier2,data=cheating,margins=F))$statistic
Tobs


B<- 10000
Tstar<-matrix(NA,nrow=B)
for (b in (1:B)){
  Tstar[b]<-chisq.test(tally(~shuffle(liar2)+copier2,data=cheating,margins=F))$statistic
}
hist(Tstar)
abline(v=Tobs,col="red",lwd=3)
plot(density(Tstar),main="Density curve of Tstar",lwd=2)
abline(v=Tobs,col="red",lwd=3)


pdata(Tobs,Tstar,lower.tail=F)



nulldist=do(10000)*chisq.test(tally(~shuffle(liar2)+copier2,data=cheating,margins=F))$statistic
names(nulldist)="Xsquared"
histogram(nulldist$Xsquared,xlab="X-sq",xlim=c(0,30),groups=nulldist>= Tobs,main="Permutation Distribution of X-sq for Cheating Data",nint=30)
ladd(panel.abline(v=Tobs,col="red",lwd=2))

pdata(Tobs,nulldist$Xsquared,lower.tail=F)

mosaicplot(cheatlietable,shade=T)

chisq.test(cheatlietable)$residuals

#Homogeneity test based on strat random sample of schools of three types from California schools:
#Population data set:
require(survey)
data(api)
require(mosaic)
tally(~stype,data=apipop)

tally(~stype,data=apistrat)
#tally(sch.wide~stype,data=apistrat)

#schooltable=tally(~stype+comp.imp,data=apistrat,margins=F)
#schooltable
#chisq.test(schooltable)
par(mfrow=c(1,2))
boxplot(growth~stype,data=apistrat,ylab="Growth",ylim=c(-55,160))
beanplot(growth~stype,data=apistrat ,log="",col="beige",method="jitter",ylim=c(-55,160))

#Growth measures the change in schools from 1999 to 2000 in the API (academic performance index)
m1<-lm(growth~stype,data=apistrat)
require(car)
Anova(m1)
plot(m1)

boxplot(residuals(lm(growth~stype,data=apistrat)))

#Cut the quantitative variable into categories 

favstats(~growth,data=apistrat)

apistrat$growthcut<-cut(apistrat$growth,breaks=c(-47,6.75,25,48,133))

plot(growthcut~stype,data=apistrat)

growthtable<-tally(~stype+growthcut,data=apistrat,margins=F)
growthtable
chisq.test(growthtable)$expected        
chisq.test(growthtable) 

chisq.test(growthtable)$residuals
        
mosaicplot(growthcut~stype,data=apistrat,shade=T)        
        
#Association test data:
data(SexualFun) #Already a contengency table
plot(SexualFun)
t1=chisq.test(SexualFun)
t1$expected
 mosaicplot(SexualFun)
 mosaicplot(SexualFun,shade=T)
mosaicplot(t1$expected)
