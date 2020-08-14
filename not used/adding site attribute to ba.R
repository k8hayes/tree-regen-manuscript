# adding site attributes to basal area
library(tidyverse)
library(here)
library(ggplot2) 
library(cowplot) 
theme_set(theme_cowplot()) 
options(scipen = 9999)

spp_ba <- read.csv(here("tree regen manuscript/spp_ba.csv"))

# organic layer depth
OL_depth <- read.csv("data/org_depth.csv")
OL_sum <-  OL_depth %>% 
  group_by(OL_depth$SITE, OL_depth$TREATMENT, OL_depth$SITE.CODE) %>% 
  summarise(AV = mean(ORG_DEPTH)) %>%
   slice(rep(1:n(), each = 4))
  spp_ba$OL_AV <- OL_sum$AV ; rm(OL_depth, OL_sum)
  
# exposed mineral soil
exp_min <- read.csv("tree regen manuscript/data/exposed_min.csv")
exp_av <- exp_min %>% 
  group_by(exp_min$SITE, exp_min$TREATMENT, exp_min$SITECODE) %>% 
  summarise(AV = mean(MINERAL)) %>%
  rename(PLOT = 'exp_min$SITECODE', TREAT = 'exp_min$TREATMENT')
  # fixing NAS
  exp_av$AV[exp_av$PLOT == "41_1"] <- 22.5
  exp_av$AV[exp_av$PLOT == "47_2"] <- 0
  # expanding each row
  exp_av <- exp_av %>% slice(rep(1:n(), each = 4))
  spp_ba$EXP_MIN <- exp_av$AV ; rm(exp_av, exp_min)

# site attributes
  site_attributes <- read.csv(here("site_attributes.csv"), stringsAsFactors = F)  
  site <- site_attributes %>%
    group_by(site_attributes$SITE, site_attributes$FIRES,
             site_attributes$SITECODE) %>%
    slice(rep(1:n(), each = 4))
spp_ba$SLOPE <- site$SLOPE ; spp_ba$SOLAR <- site$SOLAR ; rm(site, site_attributes)

write.csv(spp_ba, here("tree regen manuscript/spp_ba.csv"), row.names = F)
