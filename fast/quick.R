# Quick analysis of sims

library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(hrbrthemes)

time = seq(-20, 60, 1)
curves <- data.frame(Time=time, Chronic=time, Recovery=time, Resilient=time, Delayed=time)
curves <- curves %>%
  mutate(Recovery = )


d <- read_csv("simulations3.csv", col_types = cols())
d <- d %>% 
  select(Run, Time, PTEV, Gamma, PTES, `IMAGINAL-ACTIVATION`, NumAttributes, 
         RuminationFrequency, MemoryEntropy, ChunkSimilarity, Traumatic) %>%
  mutate(Day = floor(Time / (60*60*24)) - 100,
         Hour = floor((d$Time / 3600) %% 24),
         Gamma=as_factor(Gamma),
         RuminationFrequency=as_factor(RuminationFrequency),
         #ANS = as_factor(ANS),
         W=as_factor(`IMAGINAL-ACTIVATION`),
         PTES = as_factor(PTES),
         PTEV = as_factor(PTEV),
         NumAttributes = as_factor(NumAttributes)) %>%
  filter(Day > -20) 
  
  
ggplot(d, aes(x=Hour, fill=RuminationFrequency)) +
  geom_histogram(binwidth = 1, col="white", alpha=0.5, position="identity") +
  #geom_density(bw = 1, col="white", alpha=0.5, position="identity") +
  theme_pander()
  
# Filter data we don't care


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
  xlab("Working Memory (W)") +
  ylab("Percentage of Trajectories") +
  ggtitle("Percentage of Recovery Trajectories by WOrking Memory") +
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
  xlab("Number of Attributes (A)") +
  ylab("Percentage of Trajectories") +
  ggtitle("Percentage of Recovery Trajectories by Number of Attributes") +
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
ggplot(data=filter(a, PTEV != 1), aes(x=Day, y=Traumatic, col=NumAttributes)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", alpha=0.5) +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  theme_pander() +
  ggtitle("Effect of Number of Attributes by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)


ggplot(data=filter(a, PTEV != 1), aes(x=Day, y=Traumatic, col=W)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", alpha=0.5) +
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

ggplot(data=filter(Spatterns, PTEV != 1 & PTES != 0.75), aes(x=PTES, y=Num, fill=Trajectory)) +
  geom_bar(stat = "identity", position="stack", col="white") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  xlab("Congruency (C)") +
  ylab("Percentage of Trajectories") +  
  ggtitle("Percentage of Recovery Trajectories by Congruency") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()


ggplot(data=filter(a, PTEV != 1), aes(x=Day, y=Traumatic, col=PTES)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", alpha=0.5) +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  ggtitle("Effect of Congruency by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  #scale_color_brewer(type='seq', palette = "Blues", direction=-1) +
  #scale_color_gradient() +
  #scale_color_viridis_d(option="C") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)


Rpatterns <- patterns %>%
  #mutate(Class=as_factor(class)) %>%
  group_by(PTEV, Gamma, Trajectory, RuminationFrequency) %>%
  summarize(Num=n() / 12) %>%
  mutate(Val=cumsum(Num))


ggplot(data=filter(a, PTEV!=1), aes(x=Day, y=Traumatic, col=RuminationFrequency)) +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="errorbar", alpha=0.5) +
  #stat_summary(fun.data = mean_se, geom="point") +
  #geom_smooth(data=filter(a, Day > 0), aes(col=PTES, fill=PTES), 
  #            method = "lm") +
  facet_grid(PTEV ~ Gamma, labeller = label_both) +
  theme_pander() +
  ggtitle("Effect of Spontaneous Recollections by Gamma and PTEV") +
  ylab("Probability of Retriving a Traumatic Memory") +
  #scale_color_brewer(type='seq', palette = "GnBu") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)

ggplot(data=filter(Rpatterns, PTEV != 1), aes(x=RuminationFrequency, y=Num, fill=Trajectory)) +
  geom_bar(stat = "identity", position="stack", col="white") +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Accent") +
  xlab("Number of Spontaneous Retrievals") +
  ylab("Percentage of Trajectories") +  
  ggtitle("Percentage of Recovery Trajectories by Number of Spontaneous Retrievals") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  geom_text(aes(label=percent(Num/100)),
            position=position_stack(vjust=0.5)) +
  theme_pander()

ggplot(data=filter(a, RuminationFrequency == 0 & PTES==0), aes(x=Day, y=ChunkSimilarity, col=NumAttributes)) +
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

h_entropy <- function(timeseries, N=10) {
  L <- length(timeseries)
  mean(timeseries[L-N:N])
}

H1 <- a %>%
  filter(PTEV == 1 & Gamma == 0.8 & Day > 49) %>%
  select(MemoryEntropy) %>%
  summarise(H=mean(MemoryEntropy)) %>%
  as.double()

hsize <- a %>%
  filter(Day > 49) %>%
  group_by(Run, PTEV, PTES, PTES, NumAttributes, RuminationFrequency, W, Gamma) %>%
  summarize(H = h_entropy(MemoryEntropy) - H1,
            HippocampusDecrease = 100*(mean(MemoryEntropy)/H1 - 1),
            MeanTraumatic = mean(Traumatic),
            MeanSimilarity = mean(ChunkSimilarity))



ggplot(filter(hsize, PTEV!=1), aes(x=W, y=HippocampusDecrease, fill=RuminationFrequency)) +
  #geom_violin(col="white") +
  geom_boxplot(col="black") +
  #geom_point(position="jitter") +
  #stat_summary(fun.data = mean_se, geom="point", position = "dodge", col="red") +
  #stat_summary(fun.data = mean_sdl, geom="errorbar",  width=0.5, position=position_dodge(.9)) +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  scale_fill_brewer(palette="Blues") +
  xlab("Working Memory Capacity") +
  ylab("Percentage Decrease in Hippocampus Volume") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  theme_pander() +
  theme(panel.background = element_rect(fill=NA, color="black")) +
  annotate(geom="segment", x=-Inf, xend=Inf, y=0, yend=0) 

hsize <- hsize %>%
  mutate(Condition=paste("W=", W, "; F=", RuminationFrequency))


ggplot(filter(hsize, PTEV!=1), aes(x=MeanTraumatic, y=HippocampusDecrease, col=Condition)) +
  geom_point(alpha=.5, size=0.2) +
  #geom_density_2d() +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  scale_color_brewer(palette="Paired") +
  stat_summary(fun.data = mean_se, geom="point", size=1) +
  geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) +  theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Percentage Decrease in Hippocampus Volume") +
  xlab("Probability of Traumatic Memory Intrusion (Day 50-60)") +
  ggtitle("Symptom Severty Correlates With\nHippocampal Volume Decrease") +
  theme_pander() +
  theme(panel.background=element_rect(fill="NA", colour="black"))

ggplot(filter(hsize, PTEV!=1 & PTES==0), aes(x=MeanSimilarity, y=HippocampusDecrease, col=Condition)) +
  #geom_point(alpha=.5, size=0.2) +
  geom_density_2d() +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  scale_color_brewer(palette="Paired") +
  #geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) +  theme_pander() +
  geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Percentage Decrease in Hippocampus Volume") +
  xlab("Probability of Traumatic Memory Intrusion (Day 50-60)") +
  ggtitle("Symptom Severty Correlates With\nHippocampal Volume Decrease") +
  theme_pander() +
  theme(panel.background=element_rect(fill="NA", colour="black"))


rcorrect <- function(x) {
  if (is.na(x)) {
    0
  } else {
    x
  }
}

vrcorrect <- Vectorize(rcorrect)

hsize_r <- hsize %>%
  ungroup() %>%
  filter(PTEV != 1) %>%
  group_by(PTEV, PTES, PTES, NumAttributes, RuminationFrequency, W, Gamma, Condition) %>%
  summarize(R = cor(MeanTraumatic, HippocampusDecrease)) 

hsize_r <- hsize_r %>%
  ungroup() %>%
  mutate(R = vrcorrect(R))
#         PTES=as.numeric(as.character(PTES)))

ggplot(filter(hsize_r, PTEV!=1), aes(x=PTES, y=R, col=Condition)) +
  #geom_violin() +
  #geom_point() +
  geom_jitter(position=position_jitter(0.1)) +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  scale_color_brewer(palette="Paired") +
  ggtitle("Correlations Between Symptom Severty And \nHippocampal Volume Decrease") +
  theme_pander() +
  theme(panel.background=element_rect(fill="NA", colour="black"))



trend <- a %>% 
  filter(PTEV == 1) %>%
  group_by(Day) %>%
  summarize(Baseline = mean(MemoryEntropy))

a <- a %>%
  mutate(HippocampusSize = MemoryEntropy - trend$Baseline)