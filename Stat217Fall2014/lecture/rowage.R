age <- c(288,228,240,384,264,247,253,324,240,288,262,257,260,361,252)

row <- c(rep("1",3), rep("2",3), rep("3",3), rep("4",3), rep("5",3))
as.factor(row)

class.data <- rbind(age,row)


lm.fit <- lm(age~row)

anova(lm.fit)
summary(lm.fit)

1-pf(.41,4,14)

The following shows the F distribution with four and fifteen degrees of freedom. 
Draw a long vertical line at your F-statistic. Without even looking at the p-value,
what is your conclusion?
x <- seq(0,5,by=.01)
plot(x,df(x,4,10))

require(gplots)
plotmeans(age~row, mean.labels = T, digits = 2)
