require(Sleuth3)
handicap.data <- case0601
lm.hand <- lm(Score~Handicap, data=handicap.data)
anova(lm.hand)
require(multcomp)
cis <- glht(lm.hand, linfct=mcp(Handicap = "Tukey"))
confint(cis)
old.par <- par(mai=c(1.5,3,1,1))
plot(cis)


par(mai=c(1,1,1,1))
with(handicap.data, plot(Handicap, Score))
