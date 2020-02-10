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


a <- a %>% ungroup %>% mutate(PTEV = as_factor(PTEV),
                              A = as_factor(NumAttributes))

a <- a %>% select(Run, Day, PTEV, Gamma, PTES, W, NumAttributes, RuminationFrequency, MemoryEntropy, ChunkSimilarity, Traumatic)
a <- a %>% arrange(Run, PTEV, PTES, PTES, NumAttributes, RuminationFrequency, W, Gamma)


# Classifies people based on trajectory:
# Chronic = 1
# Recovery =2
# Delay    =3
# resilient= 4
#
classify <- function(days) {
  g1 <- days[10:19]
  g2 <- days[21:30]
  g3 <- days[70:79]
  t12 <- t.test(g1, g2)
  t23 <- t.test(g2, g3)
  category <- 0
  if (!is.na(t12$p.value) & t12$p.value < 0.05) {
    # t1 < t2
    if (!is.na(t23$p.value) & t23$p.value < 0.05) {
      # t2 <> t3
      
      if (!is.na(t23$statistic) & t23$statistic > 0) {
        # t1 < t2, t2 > 3
        category <- "Recovery" #2  # Recovery
      } else {
        # t1 < t2, t2 < t3
        category <- "Delayed" # 3 # Delayed
      } 
    } else {
      # t1 < t2, t2 = t3
      category <- "Chronic" # 1 # Chronic
    }
  } else {
    # t1 == t2
    if (!is.na(t23$p.value) & t23$p.value < 0.05) {
      # t1 == t2, t2 < t3
      category <- "Delayed" #3 # Delayed
    } else {
      # t1 == t2, t2 = t3
      category <- "Resilient" # 4 # Resilient
    }
  }
  category
}

patterns <- a %>% 
  group_by(Run, PTEV, PTES, PTES, NumAttributes, RuminationFrequency, W, Gamma) %>%
  summarize(Trajectory = classify(Traumatic)) 

cpatterns <- patterns %>%
  #mutate(Class=as_factor(class)) %>%
  group_by(PTEV, Gamma, Trajectory) %>%
  summarize(Num=n() / 24) %>%
  mutate(Val=cumsum(Num))

ggplot(data=filter(cpatterns, PTEV != 1), aes(x="", y=Num, fill=Trajectory)) +
  geom_bar(width=1, stat = "identity") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  xlab("") +
  ylab("") +
  ggtitle("Percentage of Recovery Trajectories") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()

wpatterns <- patterns %>%
  #mutate(Class=as_factor(class)) %>%
  group_by(PTEV, Gamma, Trajectory, W) %>%
  summarize(Num=n() / 8) %>%
  mutate(Val=cumsum(Num))

ggplot(data=filter(wpatterns, PTEV != 1), aes(x=W, y=Num, fill=Trajectory)) +
  geom_bar(stat = "identity", position="stack", col="white") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  xlab("") +
  ylab("") +
  ggtitle("Percentage of Recovery Trajectories") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()

Apatterns <- patterns %>%
  #mutate(Class=as_factor(class)) %>%
  group_by(PTEV, Gamma, Trajectory, NumAttributes) %>%
  summarize(Num=n() / 12) %>%
  mutate(Val=cumsum(Num))

ggplot(data=filter(Apatterns, PTEV != 1), aes(x=NumAttributes, y=Num, fill=Trajectory)) +
  geom_bar(stat = "identity", position="stack", col="white") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  xlab("") +
  ylab("") +
  ggtitle("Percentage of Recovery Trajectories") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()

ggplot(data=filter(cpatterns, PTEV != 1), aes(x="", y=Num, fill=Trajectory)) +
  geom_bar(width=1, stat = "identity") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Blues") +
  xlab("") +
  ylab("") +
  ggtitle("Percentage of Recovery Trajectories") +
  #geom_text(aes(x=1, y = Num, label=round(Num, 1))) +
  theme_pander()
  
# Plot
ggplot(data=filter(a, PTEV != 1 & PTES ==0), aes(x=Day, y=Traumatic, col=NumAttributes)) +
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
  geom_ribbon(stat=mean_se) +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  #scale_color_brewer(type='seq', palette = "Blues") +
  ggtitle("Effect of Working Memory Capacity by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)


Spatterns <- patterns %>%
  #mutate(Class=as_factor(class)) %>%
  group_by(PTEV, Gamma, Trajectory, PTES) %>%
  summarize(Num=n() / 6) %>%
  mutate(Val=cumsum(Num))

ggplot(data=filter(Spatterns, PTEV != 1), aes(x=PTES, y=Num, fill=Trajectory)) +
  geom_bar(stat = "identity", position="stack", col="white") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  #xlab("") +
  #ylab("") +
  ggtitle("Percentage of Recovery Trajectories") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()


ggplot(data=filter(a, RuminationFrequency==0), aes(x=Day, y=MemoryEntropy, col=PTES)) +
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

ggplot(data=a, aes(x=Day, y=MemoryEntropy, col=RuminationFrequency)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar") +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  #scale_color_brewer(type='seq', palette = "GnBu") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)


# Memory

trend <- a %>% 
  filter(PTEV == 1) %>%
  group_by(Day) %>%
  summarize(Baseline = mean(MemoryEntropy))

a <- a %>%
  mutate(HippocampusSize = MemoryEntropy - trend$Baseline)