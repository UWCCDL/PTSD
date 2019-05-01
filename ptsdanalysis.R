data <- read.csv("sims_2.txt")

names(data)[1] <- "Run"

data$Time <- round(data$Time/100)

data <-subset(data, data$Time < 410)

a <- aggregate(data[c("V", "Similarity")], list(Time=data$Time, PTEV=data$V_Traumatic), mean)

source("functions.R")

plot.by.2factors(a, "V", "Time", "PTEV", rng=c(0,15,2))
abline(v=31, lty=3)

plot.by.2factors(a, "Similarity", "Time", "PTEV", rng=c(0,1,0.2))
abline(v=31, lty=3)


na <- a
na$V[na$Time>180] <- 1+na$V[na$Time>180] / na$PTEV[na$Time>180]

plot.by.2factors(na, "V", "Time", "PTEV", rng=c(1,2,0.1))
abline(v=31, lty=3)
