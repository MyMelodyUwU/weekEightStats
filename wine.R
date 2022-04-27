library("tidyverse")

wine <- read.csv('wine.csv', header = TRUE)

pairs(wine, panel = panel.smooth)
wine.1 = lm(Quality ~ ., data = wine)
summary(wine.1)

wine.2 = lm(Quality ~ Clarity + Aroma + Flavour + Oakiness, data = wine)
summary(wine.2)

wine.3 = lm(Quality ~ Aroma + Flavour + Oakiness, data = wine)
summary(wine.3)

plot(wine.3, which = 1:2)

par(mfrow = c(1, 3))
plot(wine$Aroma, residuals(wine.3))
plot(wine$Flavour, residuals(wine.3))
plot(wine$Oakiness, residuals(wine.3))

summary(wine.3)$coefficients

