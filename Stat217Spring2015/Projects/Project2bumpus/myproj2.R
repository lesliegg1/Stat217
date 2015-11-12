bumpus <- read.csv("~/Documents/Stat217Spring2015/Projects/Project2bumpus/bumpus.csv", head=T)

require(mosaic)
favstats(Humerus ~ Status, data = bumpus)
Tobs <- diffmean(Humerus ~ Status, data = bumpus)
# plots
require(beanplot)
par(mfrow = c(1, 2))
beanplot(Humerus ~ Status, data = bumpus, method = "jitter",
          log = "", col = 7, ylab = "Humerus Length")
boxplot(Humerus ~ Status, data = bumpus, ylab = "Difference")
par(mfrow = c(1, 1))
# Permutation test
B <- 1000
Tstar <- matrix(NA, nrow = B)
for(b in 1:B) {
  Tstar[b] <- diffmean(Humerus ~ shuffle(Status), data = bumpus)
}
hist(Tstar, xlab = "", main = "Permuted Distribution of Differences")
abline(v = Tobs, col = "red", lwd = 2)
# pvalue
pdata(abs(Tobs), abs(Tstar), lower.tail = F)
# Use for CI and t-test
t.test(Humerus ~ Status, data = bumpus, var.equal = T)
