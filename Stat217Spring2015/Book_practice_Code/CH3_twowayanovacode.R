

#Simulated data:
require(mosaic)
#trellis.par.set(theme=col.mosaic()) 
#require(HH)

#brand=rep(c("B1","B2"),15)
#drops=sort(rep(c(10,20,30),10))
#responses=2+(as.numeric(brand=="B2"))-.2*sqrt(drops/2)+(as.numeric(brand=="B2"))*(drops==30)*(-1) +rnorm(30,0,.5)

#data1=data.frame(brand,drops,dropsf=factor(drops),responses)
pt=read.csv("http://dl.dropboxusercontent.com/u/77307195/pt.csv")

pt$drops=factor(pt$drops)
tally(~brand+drops,data=pt)


favstats(responses~brand+drops,data=pt)

require(beanplot)
beanplot(responses~brand+drops, data=pt, side = "b", col = list("lightblue", "white"),xlab="Drops",ylab="Time",,method="jitter",log="")
legend("topright", bty="n",c("B1", "B2"), fill = c("lightblue", "white"))

#For cover:

beanplot(responses~brand+drops, data=pt, side = "b", col = list("white", "lightblue"),horizontal=T)
legend("topright", bty="n",c("B1", "B2"), fill = c("white", "lightblue"))



interaction.plot(responses~brand*drops,data=pt)


source("http://dl.dropboxusercontent.com/u/77307195/intplot.R")

par(mfrow=c(2,1))
intplot(responses~brand*drops,data=pt)
intplot(responses~drops*brand,data=pt)

lm1=lm(responses~brand*dropsf,data=pt)
res1=residuals(lm1)

par(mfrow=c(2,2))
#Scenario 1: No effects
m1=lm(responses~1,data=pt)
pt$r1=fitted(m1)+res1
intplot(r1~brand*drops,ylim=c(0,3.2),data=pt,main="(a) Scenario 1: No effects",col=c(1,2),lwd=2)
#Scenario 2: A effect only
m2=lm(responses~brand,data=pt)
pt$r2=fitted(m2)+res1
intplot(r2~brand*drops,ylim=c(0,3.2),data=pt,main="(b) Scenario 2: A only",col=c(1,2),lwd=2)

#Scenario 3: B effect only
m3=lm(responses~drops,data=pt)
pt$r3=fitted(m3)+res1
intplot(r3~brand*drops,ylim=c(0,3.2),data=pt,main="(c) Scenario 3: B only",col=c(1,2),lwd=2)

#Scenario 4: A and B additive
m4=lm(responses~brand+drops,data=pt)
pt$r4=fitted(m4)+res1
intplot(r4~brand*drops,ylim=c(0,3.2),data=pt,main="(d) Scenario 4: A and B additive",col=c(1,2),lwd=2)

#Scenario 5: A and B interaction
m5=lm(responses~brand*drops,data=pt)
pt$r5=fitted(m5)+res1
par(mfrow=c(1,1))
intplot(r5~brand*drops,ylim=c(0,3.2),data=pt,main="Scenario 5: Interaction between A and B",col=c(1,2),lwd=2)


m1=lm(responses~brand*drops,data=pt)
anova(m1)
require(effects)
plot(allEffects(m1),grid=T,multiline=T,ci.style="bars")
summary(m1)

m2=lm(responses~brand+drops,data=pt)
anova(m2)
require(effects)
plot(allEffects(m2))
summary(m2)

require(car)
Anova(m2)

data(ToothGrowth)

par(mfrow=c(1,2))
beanplot(len~supp*dose, data=ToothGrowth, side = "b",ylim=c(-5,40),main="Beanplot" ,col = list("white","orange"),xlab="Dosage",ylab="Tooth Growth")
legend("topright", bty="n",c("VC", "OJ"), fill = c("white", "orange"))

intplot(len~supp*dose,data=ToothGrowth,col=c(1,2),main="Interaction Plot",ylim=c(-5,40))

TG1 <- lm(len~supp*dose,data=ToothGrowth)
Anova(TG1)

ToothGrowth$dosef=factor(ToothGrowth$dose)
TG2 <- lm(len~supp*dosef,data=ToothGrowth)
Anova(TG2) 

par(mfrow=c(2,2))
plot(TG2) 


pf(4.107,df1=2,df2=54,lower.tail=F)

summary(TG2)


require(effects)
plot(allEffects(TG2),grid=T,multiline=T,ci.style="bars")

par(mfrow=c(1,1))
intplot(len~supp*dose,data=ToothGrowth,col=c(1,2),cldshift=1,cld=T,main="Interaction Plot with CLD")


require(faraway)
data(debt)
debt$incomegp<-factor(debt$incomegp)
debt$cigbuy<-factor(debt$cigbuy)
debtc<-na.omit(debt)

intplot(prodebt~cigbuy*incomegp,data=debtc,col=c(1,3),lwd=2)

tally(~incomegp+cigbuy,data=debtc)

require(car)
debt1<-lm(prodebt~incomegp*cigbuy,data=debtc)
Anova(debt1)
par(mfrow=c(2,2))
plot(debt1)
debt1r<-lm(prodebt~incomegp+cigbuy,data=debtc)
plot(debt1r)
Anova(debt1r)
require(effects)
plot(allEffects(debt1r))

summary(debt1r)

par(mfrow=c(1,2))
intplot(jitter(fitted(debt1r))~cigbuy*incomegp,data=debtc,col=c(1,3),lwd=2,main="(a)")
intplot(jitter(fitted(debt1r))~incomegp*cigbuy,data=debtc,lwd=2,main="(b)")




debt$house=factor(debt$house)

intplot(prodebt~incomegp*house,data=debtc)
tally(~incomegp+house,data=debtc)
debt3<-lm(prodebt~incomegp*house,data=debtc)
Anova(debt3)

debt3r<-lm(prodebt~incomegp+house,data=debtc)
Anova(debt3r)

#Unreplicated version of water and paper towels:
ptR<-read.csv("http://dl.dropboxusercontent.com/u/77307195/ptR.csv")
ptR$dropsf<-factor(ptR$drops)
ptR
intplot(responses~brand*dropsf,data=ptR,lwd=2)
anova(lm(responses~dropsf*brand,data=ptR))
require(car)
norep1<-lm(responses~dropsf+brand,data=ptR)
Anova(norep1)

require(effects)
plot(allEffects(norep1))

#Randomized block analysis:




