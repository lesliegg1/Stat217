install.packages("effects")
require(effects)
require(mosaic)
data(TitanicSurvival)
ship<-tally(~survived+passengerClass,data=TitanicSurvival)
ship
mosaicplot(t(ship))

chisq.test(ship)
chisq.test(ship)$expected

chisq.test(ship)$residuals
mosaicplot(t(ship),shade=T)

install.packages("tabplot")
require(tabplot)
tableplot(TitanicSurvival[,c(1,4,2,3)])
