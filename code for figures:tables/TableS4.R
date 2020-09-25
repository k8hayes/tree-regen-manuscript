library(ggplot2)
library(cowplot)
library(tidyverse)
theme_set(theme_cowplot())
library(here)

regen <- read.csv(here("all.csv"))
regen$SPP_PROP <- regen$SPP_COUNT_PLOT / regen$TOTAL_COUNT_PLOT

meanRegen <- regen %>% 
  group_by(regen$TREAT, regen$SITE, regen$SPP) %>% 
  summarise(MEAN_PROP = mean(SPP_PROP), SD = sd(SPP_PROP))  %>%
  complete(regen$SPP, fill = list(MEAN_PROP = 0, SD = 0))
meanRegen <- as.data.frame(meanRegen)
colnames(meanRegen) <- c( "TREAT", "SITE", "SPP", "MEAN_PROP", "SD")

meanRegen <- meanRegen[meanRegen$SPP != "POBA",]
meanRegen <- meanRegen[meanRegen$SPP != "PIGL",]
meanRegen <- meanRegen[meanRegen$SPP != "ARCTO",]
meanRegen <- meanRegen[meanRegen$SPP != "ARCT",]
meanRegen <- meanRegen[meanRegen$SPP != "ALCR",]

meanRegen$MEAN_PROP <- meanRegen$MEAN_PROP * 100
meanRegen$SD <- meanRegen$SD *100

meanRegen$MEAN_PROP <- round(meanRegen$MEAN_PROP, digits = 1)
meanRegen$SD <- round(meanRegen$SD, digits = 1)
