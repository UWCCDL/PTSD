# Quick analysis of sims

library(ggplot2)

d <- read.csv("test-automatic.txt")
d$Day <- floor(d$Time / (60*60*24)) - 100
#d$Day <- as.factor(d$Day)
d <- subset(d, d$Day > -10)
d$PTEV <- as.factor(d$PTEV)

a <- aggregate(d[c("Traumatic", "ChunkV")], list(Day=d$Day, PTEV=d$PTEV), mean)

#a$PTEV <-as.factor(a$PTEV)



ggplot(a, aes(x=Day, y=Traumatic, col=PTEV)) +
  #geom_line()
  stat_summary(fun.data = mean_se, geom="line") +
  #stat_summary(fun.data = mean_se, geom="errorbar") +
  #stat_sum_df("median_hilow", mapping = aes(group = cyl))+
  theme_light()

