faith.data <- read.csv("~/Documents/Stat217Fall2014/Projects/Project3/faith.data.csv")

with(faith.data,plot(INTERVAL~DURATION))
lm.out <- lm(INTERVAL~DURATION, data=faith.data)
summary(lm.out)
with(faith.data, plot(DURATION,INTERVAL,type="n", xlim=c(1,6),
                      main="Waiting time vs. Duration"))
with(faith.data, points(DURATION,INTERVAL, pch=16))
abline(lm.out)
abline(h=seq(40,90,by=10), lty=2)
par(mfrow=c(2,2))
plot(lm.out)
confint(lm.out)
dur.2 <- with(faith.data, data.frame(DURATION=2))
fit.2 <- predict(lm.out, newdata=dur.2, se.fit=TRUE, interval="confidence",level=0.95)

dur.2 <- with(faith.data, data.frame(DURATION=2))
fit.2 <- predict(lm.out, newdata=dur.2, se.fit=TRUE, interval="confidence",level=0.95)
fit.2

dur.4 <- with(faith.data, data.frame(DURATION=4))
pred.4 <- predict(lm.out, newdata=dur.4, se.fit=TRUE, interval="prediction", level=0.95)
pred.4

### The following is the code for the plot with 95% confidence interval and
###prediction interval bands
###Highlight this whole chunk of code and run it all at once
new <- data.frame(faith.data$DURATION = seq(1.7, 4.9, length=107))
#50 values between -220 and 1090
est.mean.cis <- predict(lm.out, newdata=new, interval="confidence")
pred.pis <- predict(lm.out, newdata=new, interval="prediction")
## Make a confidence BAND using the Scheffe multiplier
est.mean.out <- predict(lm.out, newdata=new, se.fit=TRUE, interval="confidence")
est.mean.ses <- est.mean.out$se.fit
look <- est.mean.out$fit
est.means <- est.mean.out$fit[,1] #takes first column of the $fit matrix
#which are the fitted means for each of the 50 values
sch <- (sqrt(2*qf(.95,2,105))) #this is the Scheffe multiplier
tmult <- qt(.975,105) #compare to the t-multiplier
conf.BAND.Scheffe.low <- est.means - (sqrt(2*qf(.95,2,105)))*est.mean.ses
conf.BAND.Scheffe.hi <- est.means + (sqrt(2*qf(.95,2,105)))*est.mean.ses
sch.band <- cbind(conf.BAND.Scheffe.low, conf.BAND.Scheffe.hi)
with(faith.data, plot(DURATION, INTERVAL, type="n", ylab="Interval (minutes)",
                      xlab="Duration (minutes)", main="Old Faithful Data"))
abline(lm.out, lwd=2) #fitted line
lines(new$DURATION, est.mean.cis[,1], lty=1, lwd=2) #another way to add fitted line
lines(new$DURATION, est.mean.cis[,2], lty=2, lwd=2, col=2) #lower pointwise CI
lines(new$DURATION, est.mean.cis[,3], lty=2, lwd=2, col=2) #upper pointwise CI
lines(new$DURATION, pred.pis[,2], lty=1, lwd=2, col=3) #lower prediction PI
lines(new$DURATION, pred.pis[,3], lty=1, lwd=2, col=3) #upper prediction PI
lines(new$DURATION, conf.BAND.Scheffe.low, lty=1, lwd=2, col=4) #lower confidence band
lines(new$DURATION, conf.BAND.Scheffe.hi, lty=1, lwd=2, col=4) #upper confidence band
with(faith.data, points(DURATION, INTERVAL, pch=16, cex=0.5))
