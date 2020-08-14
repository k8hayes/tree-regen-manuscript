exp_min <- read.csv(here("data/exposed_min.csv"))
org_depth <- read.csv("data/org_depth.csv")
library(tidyverse)


ave <- exp_min %>%
  rename(TREAT = TREATMENT) %>%
  drop_na(MINERAL) %>%
  group_by(SITE, TREAT, SITECODE) %>%
  summarise(mean(MINERAL))

ave1 <- org_depth %>%
  rename(TREAT = TREATMENT) %>%
  group_by(SITE, TREAT, SITE.CODE) %>%
  summarise(mean(ORG_DEPTH))

ave$ORG_DEPTH <- ave1$`mean(ORG_DEPTH)`            

ave <- ave %>%
  rename(EXPOSED_MINERAL = mean(MINERAL))
write.csv(ave, "soil_characteristics.csv", row.names = F)
