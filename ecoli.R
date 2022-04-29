library("tidyverse")
library("devtools")
library("glue")
library("dplyr")
library("stringr")
library("ggplot2")
library("knitr")
library("ggrepel")
library("patchwork") library("janitor")
library("usethis")
library("roxygen2")
library("testthat")

ecoli.dat <- read.table("ecoli.txt", header=TRUE)

plot(ecoli.dat$HEC ~ ecoli.dat$HGMF)

# create linear model from the plot, and use abline function to write line
abline(lm(ecoli.dat$HEC ~ ecoli.dat$HGMF))
# the data looks like it has a linear trend

summary(lm(ecoli.dat$HEC ~ ecoli.dat$HGMF))
#since we now have linear model, we cna plot it out

plot(lm(ecoli.dat$HEC ~ ecoli.dat$HGMF), which = 1:2)
# there is a large outlier and the right plot seems left skewed

# do we need to use predict?
#anova(lm(ecoli.dat$HEC ~ ecoli.dat$HGMF), data = ecoli.dat)

# ------------------------------------------------------------------------

par(mfrow = c(1, 2))

plot(lm(ecoli.dat$HEC ~ ecoli.dat$HGMF), which = 1:2)

#two outliers outside of IQR

# or we can do:

# take residuals from linear model

test <- lm(ecoli.dat$HEC ~ ecoli.dat$HGMF)$residuals

# check IQR
findOutlier <- which(test < -0.5)

removeOutlier = ecoli.dat[-findOutlier]

plot(removeOutlier)

a <- lm(removeOutlier)

plot(fitted(a), residuals(a))

plot(a, which = 2)








