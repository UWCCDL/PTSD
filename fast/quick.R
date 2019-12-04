# Quick analysis of sims

library(tidyverse)
library(ggplot2)
library(ggthemes)

d <- read_csv("test-rumination.csv", col_types = cols())
d <- d %>%
  mutate(Day = floor(Time / (60*60*24)) - 100,
         Hour = floor((d$Time / 3600) %% 24))

ggplot(d, aes(x=Hour, fill=RuminationFrequency)) +
  #geom_histogram(binwidth = 1, col="white", alpha=0.5, position="identity") +
  geom_density(bw = 1, col="white", alpha=0.5, position="identity") +
  theme_pander()
  
# Filter data we don't care
d <- d %>% filter(Day > -20)
d <- d%>% mutate(#RT = as_factor(RT), 
                 Gamma=as_factor(Gamma),
                 RuminationFrequency=as_factor(RuminationFrequency),
                 #ANS = as_factor(ANS),
                 PTES = as_factor(PTES))

# Reduce, summarizing across events 
a <- d %>% 
  group_by(Day, PTES, Run, Gamma, RuminationFrequency) %>% 
  summarize_all(mean) 

#a <- aggregate(d[c("Traumatic", "ChunkV")], list(Day=d$Day, PTEV=d$PTEV, Run=d$Run, Gamma=d$Gamma), mean)
#a<- as_tibble(a)
#a$PTEV <-as.factor(a$PTEV)
#a$Gamma <- as.factor(a$Gamma)

# Plot
ggplot(data=a, aes(x=Day, y=Traumatic, col=RuminationFrequency)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar") +
  stat_summary(fun.data = mean_se, geom="point") +
  geom_smooth(data=filter(a, Day > 0), aes(col=RuminationFrequency, fill=RuminationFrequency), 
              method = "lm") +
  theme_pander() +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="blue", alpha=0.2)

