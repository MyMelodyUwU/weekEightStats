library("tidyverse")

birch <- read.table('birch.dat', header = TRUE)


pairs(birch, panel = panel.smooth)
cor(birch)
birch.lm = lm(Response ~ Cyield + BEC + pH, data = birch)
summary(birch.lm)
coefficients(birch.lm)

birch.lm = lm(Response ~ Cyield + BEC + pH, data = birch)
anova(birch.lm)

birch.lm = lm(Response ~ Cyield + BEC + pH, data = birch)
birch.aov = anova(birch.lm)
RegSS = birch.aov$`Sum Sq`[1:3]
birch.aov

birch.aov = anova(lm(Response ~ BEC + pH + Cyield, data = birch))
RegSS = birch.aov$`Sum Sq`[1:3]
birch.aov

birch.aov = anova(lm(Response ~ BEC + pH + Cyield, data = birch))
RegSS = birch.aov$`Sum Sq`[1:3]
birch.aov

birch.lm = lm(Response ~ Cyield + BEC + pH, data = birch)
summary(birch.lm)

summary(lm(Response ~ Cyield, data = birch))

summary(lm(Response ~ BEC, data = birch))

summary(lm(Response ~ pH, data = birch))

summary(lm(y ~ x1 + x2, data = multidat))


anova(lm(y ~ x1 + x2, data = multidat))


anova(lm(y ~ x2 + x1, data = multidat))

summary(lm(y ~ x1, data = multidat))$coefficients

summary(lm(y ~ x2, data = multidat))$coefficients

summary(lm(Response ~ Cyield + pH + BEC, data = birch))$coefficients

summary(lm(Response ~ Cyield + pH, data = birch))$coefficients

summary(lm(Response ~ Cyield, data = birch))

par(mfrow = c(1, 2))
mylm <- lm(Response ~ Cyield + pH, data = birch)
qqnorm(mylm$residuals, main = "Normal Q-Q plot of residuals")
plot(mylm$fitted, mylm$residuals, main = "Residuals vs Fitted",
     xlab = "Fitted", ylab = "Residuals")

par(mfrow = c(1, 2))
plot(birch$Cyield, mylm$residuals, main = "Residuals vs Cyield",
     xlab = "Cyield", ylab = "Residuals")
plot(birch$pH, mylm$residuals, main = "Residuals vs pH",
     xlab = "pH", ylab = "Residuals")

