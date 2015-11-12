require(mosaic)
require(beanplot)
require(multcomp)
mice <- read.csv("~/Documents/Stat217Fall2015/Projects/project2/mice.csv") ## insert the path for your saved mice.csv file
head(mice) ## shows you the first 6 lines of your mice data, so you can see how it is organized
favstats(weeks ~ dose, data = mice)
beanplot(weeks ~ dose, data = mice, method = "jitter", log = "", col = 7)
mice.lm <- lm(weeks ~ dose, data = mice)
summary(mice.lm)
anova(mice.lm)
par(mfrow = c(2, 2)) ## arranges the diagnostic plots in a 2x2 grid
plot(mice.lm)
mice.lm2 <- glht(mice.lm, linfct = mcp(dose = "Tukey"))
confint(mice.lm2, level = 0.95)
par(mfrow = c(1, 1)) ## makes only one plot appear at a time
old.par <- par(mai = c(1.5, 2.5, 1, 1)) ## makes room on the plot for the group names
plot(mice.lm2)
cld(mice.lm2)

