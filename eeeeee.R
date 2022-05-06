meter <- read.csv("flow_meter.csv", header=TRUE)

mylm=lm(weight~pressure, data=meter)
mylm$coefficients[1]
