#library(matlab)
data <- read.csv("sims_3.txt")

names(data)[1] <- "Run"

data$Time <- round(data$Time/100)

#data <-subset(data, data$Time < 410)

data$Block<-round(data$Time/25, 0)

data$Block <- data$Block - 6

a <- aggregate(data[c("Chunk", "ChunkSimilarity")],
               list(Block=data$Block, PTEV=data$PTEV,
                    S=data$PTES, W=data$IMAGINAL.ACTIVATION,
                    BLL=data$BLL),
               mean)

source("functions.R")
library(ggplot2)

ggplot(a, aes(x=Block, y=Chunk, col=PTEV)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="point") +
  stat_summary(fun.data = mean_se, geom="errorbar", width=.05) +
  facet_wrap(~W * ~BLL) +
  theme_linedraw()

plot.by.2factors(data, "V", "Block", "V_Traumatic", rng=c(0,12,2), legpos = "topleft",
                 colors = jet.colors(5))
abline(v=7, lty=3)

plot.by.2factors(a, "V", "Block", "PTEV", rng=c(0,12,2), legpos = "topleft",
                 colors = jet.colors(5))
abline(v=7, lty=3)

plot.by.2factors(a, "Similarity", "Block", "PTEV", rng=c(0,0.4,0.2))
abline(v=7, lty=3)

norm <- tapply(a$V, a$PTEV, max)
na <- a
for (p in unique(na$PTEV)) {
  na$V[na$Block>0 & na$PTEV == p] <- na$V[a$Block>0 & a$PTEV == p] / max(a$V[na$Block>6 & a$PTEV == p])
}
plot.by.2factors(na, "V", "Block", "PTEV", rng=c(0,1,0.1))
abline(v=7, lty=3)


