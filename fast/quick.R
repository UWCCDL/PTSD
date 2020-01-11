# Quick analysis of sims

library(tidyverse)
library(ggplot2)
library(ggthemes)

d <- read_csv("simulations3.csv", col_types = cols())
d <- d %>%
  mutate(Day = floor(Time / (60*60*24)) - 100,
         Hour = floor((d$Time / 3600) %% 24))
names(d)[18] <- "W"

ggplot(d, aes(x=Hour, fill=RuminationFrequency)) +
  geom_histogram(binwidth = 1, col="white", alpha=0.5, position="identity") +
  #geom_density(bw = 1, col="white", alpha=0.5, position="identity") +
  theme_pander()
  
# Filter data we don't care
d <- d %>% filter(Day > -20)
d <- d%>% mutate(#RT = as_factor(RT), 
                 Gamma=as_factor(Gamma),
                 RuminationFrequency=as_factor(RuminationFrequency),
                 #ANS = as_factor(ANS),
                 W=as_factor(W),
                 PTES = as_factor(PTES),
                 PTEV = as_factor(PTEV),
                 NumAttributes = as_factor(NumAttributes))

# Reduce, summarizing across events 
a <- d %>% 
  group_by(Day, PTES, PTEV, Run, Gamma, W, RuminationFrequency, NumAttributes) %>% 
  summarize_all(mean) 


a <- a %>% ungroup %>% mutate(PTEV=as_factor(PTEV), A=as_factor(NumAttributes))

#a <- aggregate(d[c("Traumatic", "ChunkV")], list(Day=d$Day, PTEV=d$PTEV, Run=d$Run, Gamma=d$Gamma), mean)
#a<- as_tibble(a)
#a$PTEV <-as.factor(a$PTEV)
#a$Gamma <- as.factor(a$Gamma)

# Plot
ggplot(data=filter(a, PTEV != 1), aes(x=Day, y=Traumatic, col=NumAttributes)) +
  stat_summary(fun.data = mean_se, geom="line") +
  #stat_summary(fun.data = mean_se, geom="errorbar") +
  stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  theme_pander() +
  ggtitle("Effect of Number of Attributes by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="blue", alpha=0.2)


ggplot(data=a, aes(x=Day, y=Traumatic, col=W)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar") +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  #scale_color_brewer(type='seq', palette = "Blues") +
  ggtitle("Effect of Number of Attributes by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)

ggplot(data=filter(a, RuminationFrequency==0), aes(x=Day, y=Traumatic, col=PTES)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar") +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma) +
  theme_pander() +
  #scale_color_brewer(type='seq', palette = "Blues", direction=-1) +
  #scale_color_gradient() +
  #scale_color_viridis_d(option="C") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)


ggplot(data=a, aes(x=Day, y=Traumatic, col=RuminationFrequency)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar") +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  #scale_color_brewer(type='seq', palette = "GnBu") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)

