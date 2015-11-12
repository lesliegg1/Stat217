require(PairedData)
data(PrisonStress)
stress <- PrisonStress
head(stress)
tail(stress)
stress$Differences<-PrisonStress$PSSafter-PrisonStress$PSSbefore
stress
stress$Differences
mean(stress$Differences)
sd(stress$Differences)
summary(stress$Differences)
hist(stress$Differences)
boxplot(stress$Differences)
