library("tidyverse")

housedat <- read.table('housedata.txt', header = TRUE)

summary(lm(price ~ housearea, data = housedat))

summary(lm(price ~ landarea, data = housedat))

summary(lm(price ~ housearea + landarea, data = housedat))

