# Quick analysis of sims

library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(viridis)
#library(hrbrthemes)

a <- read_csv(gzfile("../simulations/simulation3_aggregated.csv.gz"),
              col_types = cols())

a <- a %>%
  rename(I = PTEV) %>%
  rename(gamma = Gamma) %>%
  mutate(I = as_factor(I)) %>%
  mutate(gamma = as_factor(gamma)) %>%
  mutate(W = as_factor(W))


ggplot(filter(a, I != 1), aes(x=Day, y=Traumatic, col=I)) +
  #geom_point(alpha=.05, size=0.1) +
  #geom_density_2d() +
  scale_color_jco() +
  facet_grid(~ W, labeller=label_both) +
  #scale_color_brewer(palette="Dark2") +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="point", alpha=0.25) +
  stat_summary(fun.data = mean_cl_boot, geom="errorbar", alpha = 0.5) +
  #geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) +  theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Probability of Traumatic Memory Intrusion") +
  xlab("Day") +
  ggtitle("Frequency of Memory Intrusions\nFollowing a Traumatic Event") +
  #annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alphaa=0.2) +
  annotate("segment", x=-0.5, xend=-0.5,
           y=-Inf, yend=Inf, col="black", lty=2, size=1) +
  annotate("rect", xmin=50, xmax=60,
           ymin=-Inf, ymax=Inf, fill="red", alpha=0.2)+
  theme_pander() +
  theme(legend.position = "bottom") #+
  #theme(panel.background=element_rect(fill="NA", colour="black"))

ggplot(filter(a, I != 1), aes(x=Day, y=Traumatic, col=gamma)) +
  #geom_point(alpha=.05, size=0.1) +
  #geom_density_2d() +
  facet_grid(W~I, labeller=label_both) +
  #scale_color_brewer(palette="Dark2") +
  scale_color_d3() +
  stat_summary(fun.data = mean_se, geom="line") +
  stat_summary(fun.data = mean_se, geom="point", alpha=0.25) +
  stat_summary(fun.data = mean_se, geom="errorbar", alpha = 0.5) +
  #geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) +  theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Probability of Traumatic Memory Intrusion") +
  xlab("Day") +
  ggtitle("Frequency of Memory Intrusions Following PTE") +
  annotate("rect", xmin=-2, xmax=2, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +

  #annotate("rect", xmin=50, xmax=60,
  #         ymin=-Inf, ymax=Inf, fill="black", alpha=0.2)+
  theme_pander() +
  #theme(legend.position = "bottom") +
  theme(panel.background=element_rect(fill="NA", colour="black"))



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
  group_by(Run, I, PTES, NumAttributes, RuminationFrequency, W, gamma) %>%
  summarize(Trajectory = classify(Traumatic))

patterns <- patterns %>% ungroup

pattern_count <- patterns %>%
  group_by(I, gamma, Trajectory, NumAttributes,
           RuminationFrequency, PTES, W) %>%
  summarize(Proportion = n() / 50)

W_patterns <- patterns %>%
  group_by(I, Trajectory, W) %>%
  summarize(Num=n() / 24) %>%
  mutate(Val=cumsum(Num))

ggplot(data=filter(W_patterns, I != 1), aes(x="", y=Num/100, fill=Trajectory)) +
  geom_bar(stat = "identity", col="white", width=1) +
  facet_grid(W ~ I, labeller=label_both) +
  coord_polar("y", start=0) +
  scale_fill_d3() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2L)) +
  #scale_fill_brewer(palette="Accent") +
  xlab("Working Memory (W)") +
  ylab("Percentage of Trajectories") +
  ggtitle("Predicted Percentage of Recovery Trajectories") +
  geom_text(aes(label=percent(Num/100, .1)),
            position=position_stack(vjust=0.5)) +
  theme_pander()





# Memory

h_entropy <- function(timeseries, N = 10) {
  L <- length(timeseries)
  mean(timeseries[L-N:N])
}

H1 <- a %>%
  filter(I == 1 & gamma == 0.8 & Day > 49) %>%
  select(MemoryEntropy) %>%
  summarise(H=mean(MemoryEntropy)) %>%
  as.double()

hsize <- a %>%
  filter(Day > 49) %>%
  group_by(Run, I, PTES, NumAttributes, RuminationFrequency, W, gamma) %>%
  summarize(H = h_entropy(MemoryEntropy) - H1,
            HippocampusDecrease = 100*(mean(MemoryEntropy)/H1 - 1),
            MeanTraumatic = mean(Traumatic),
            MeanSimilarity = mean(ChunkSimilarity))


ggplot(filter(hsize, I!=1), aes(x=W, y=HippocampusDecrease, fill=RuminationFrequency)) +
  #geom_violin(col="white") +
  geom_boxplot(col="black") +
  #geom_point(position="jitter") +
  #stat_summary(fun.data = mean_se, geom="point", position = "dodge", col="red") +
  #stat_summary(fun.data = mean_sdl, geom="errorbar",  width=0.5, position=position_dodge(.9)) +
  facet_grid(I ~ Gamma, labeller=label_both) +
  #coord_polar("y", start=0) +
  #scale_fill_brewer(palette="Blues") +
  #scale_fill_brewer(palette="Blues") +
  xlab("Working Memory Capacity") +
  ylab("Percentage Decrease in Hippocampus Volume") +
  #geom_text(aes(y = Val-Num/2, label=percent(Num/100))) +
  theme_pander() +
  theme(panel.background = element_rect(fill=NA, color="black")) +
  annotate(geom="segment", x=-Inf, xend=Inf, y=0, yend=0)

hsize <- hsize %>%
  mutate(Condition=paste("W=", W, "; F=", RuminationFrequency))

ptev_val <- function(val) {
  paste("I =", val)
}

gamma_val <- function(val) {
  #expression(paste(gamma, "=", val))
  paste("gamma =", val)
}

gval <- as_labeller(gamma_val)

ggplot(filter(hsize, I != 1),
       aes(x= I, y = HippocampusDecrease, fill = I)) +
  facet_grid(~ W, labeller = label_both) +
  scale_fill_brewer(palette="Dark2") +
  stat_summary(fun.data = "mean_se", geom="bar", position = "dodge") +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar",
               position = "dodge", width=0.25) +
  theme_pander() +
  ggtitle("Effects of Trauma Intensity\non Hippocampal Volume") +
  ylab("Percent Decrease in Hippocampus Volume") +
  xlab("Intensity of Traumatic Event (I)") +
  theme(legend.position = "bottom")

ggplot(filter(hsize, I != 1),
       aes(x= gamma, y = HippocampusDecrease, fill = gamma, col=gamma)) +
  facet_grid(W ~ I, labeller = label_both) +
  geom_violin(alpha=0.25, col="white") +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  stat_summary(fun.data = mean_se, geom="point", aes(col=gamma), size=3) +
  stat_summary(fun.data = mean_sdl, geom="errorbar",
               aes(col=gamma), width=0.05) +
  #stat_summary(fun.data = "mean_se", geom="bar", position = "dodge") +
  #stat_summary(fun.data = "mean_cl_boot", geom="errorbar",
  #             position = "dodge", width=0.25) +
  theme_pander() +
  ggtitle("Effects of Trauma Intensity\non Hippocampal Volume") +
  ylab("Percent Decrease in Hippocampus Volume") +
  xlab("Intensity of Traumatic Event (I)") +
  theme(legend.position = "bottom")

ggplot(filter(hsize, I != 1),
       aes(x=MeanTraumatic, y=HippocampusDecrease, col = gamma)) +
  geom_point(alpha=.75, size=0.1) +
  #geom_density_2d() +
  facet_grid(W ~ I, labeller=label_both) +
  #scale_color_brewer(palette="Paired") +
  scale_color_d3() +
  stat_summary(fun.data = mean_se, geom="point", size=1) +
  geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) + # theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Percentage Decrease in Hippocampus Volume") +
  xlab("Probability of Traumatic Memory Intrusion (Day 50-60)") +
  ggtitle("Symptom Severity And Hippocampal Volume Decrease") +
  theme_pander() +
#  theme(legend.position = "bottom") +
  theme(panel.background=element_rect(fill="NA", colour="black"))

ggplot(filter(hsize, I != 1),
       aes(x=MeanTraumatic, y=HippocampusDecrease, col = W)) +
  geom_point(alpha=.75, size=0.1) +
  #geom_density_2d() +
  facet_grid(I ~ gamma, labeller=label_both) +
  scale_color_brewer(palette="Paired") +
  stat_summary(fun.data = mean_se, geom="point", size=1) +
  geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) + # theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Percentage Decrease in Hippocampus Volume") +
  xlab("Probability of Traumatic Memory Intrusion (Day 50-60)") +
  ggtitle("Symptom Severity\nAnd Hippocampal Volume Decrease") +
  theme_pander() +
  theme(legend.position = "bottom") +
  theme(panel.background=element_rect(fill="NA", colour="black"))


## Figure 4
##
ggplot(filter(hsize, I != 1),
       aes(x=MeanTraumatic, y=HippocampusDecrease, col = I)) +
  geom_point(alpha = .1, size=1) +
  #geom_density_2d() +
  #scale_color_viridis(option="Dark2", discrete=T) +
  scale_color_brewer(palette="Dark2") +
  #stat_summary(fun.data = mean_se, geom="point", size=1) +
  geom_smooth(method = "lm", formula = y ~ x, aes(col=I), fill="white",
              fullrange= T) + # theme_pander() +
  #geom_smooth(method = "lm", aes(fill=Condition), se=F) +
  ylab("Percentage Decrease in Hippocampus Volume") +
  xlab("Probability of Traumatic Memory Intrusion (Day 50-60)") +
  ggtitle("Correlation Between Symptom Severity\nAnd Hippocampal Volume Decrease") +
  theme_pander() +
  theme(legend.position = "bottom") #+
  #theme(panel.background=element_rect(fill="NA", colour="black"))


ggplot(filter(hsize, PTEV!=1 & PTES==0), aes(x=MeanSimilarity,
                                             y=HippocampusDecrease,
                                             col=W)) +
  #geom_point(alpha=.5, size=0.2) +
  geom_density_2d() +
  facet_grid(PTEV ~ Gamma, labeller=label_both) +
  scale_color_brewer(palette="Dark2") +
  #geom_smooth(method = "lm", formula = y ~ x, col="black", fullrange= T) +  theme_pander() +
  geom_smooth(method = "lm", aes(fill=W), se=F) +
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
