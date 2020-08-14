# DO NOT USE
# DOES THIS WRONG


# basal area
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

daltDBH <- read.csv(here("data/dalton_ClumpDBH_NoZero.csv"), stringsAsFactors = F)
steeseDBH <- read.csv(here("data/DBH/steese_ClumpDBH_NoZero.csv"))
      #  unique(daltDBH$PLOT)
      #  daltDBH$PLOT[daltDBH$PLOT == "12_1_TLS"] <- "12_1"
      #  daltDBH$PLOT[daltDBH$PLOT == "50_1_TLS"] <- "50_1"
      #  daltDBH$PLOT[daltDBH$PLOT == "64_1_TLS"] <- "64_1"
      #  daltDBH$PLOT[daltDBH$PLOT == "65_1_TLS"] <- "65_1"
      #  write.csv(daltDBH, here("data/DBH/dalton_ClumpDBH_NoZero.csv"), row.names = F)

dbh <- rbind(steeseDBH, daltDBH) ; rm(daltDBH, steeseDBH)

dbh$SPP[dbh$SPP == "SAGL"] <- "SALIX"
dbh$SPP[dbh$SPP == "SA_3"] <- "SALIX"
dbh$SPP[dbh$SPP == "SA_5"] <- "SALIX"
dbh$SPP[dbh$SPP == "SA_6"] <- "SALIX"
dbh$SPP[dbh$SPP == "SA_7"] <- "SALIX"
dbh$SPP[dbh$SPP == "SA_8"] <- "SALIX"
dbh$SPP[dbh$SPP == "SADE"] <- "SALIX"
dbh$SPP[dbh$SPP == "SAPU"] <- "SALIX"
dbh$SPP[dbh$SPP == "SAGL_R"] <- "SALIX"
write.csv(dbh, "dbh.csv", row.names = F)

dbh <- read.csv("dbh.csv")
dbh <- dbh[dbh$SPP != "ARCT",]
dbh <- dbh[dbh$SPP != "POBA",]
dbh <- dbh[dbh$SPP != "ALCR",]
write.csv(dbh, "dbh.csv", row.names = F)

dbh <- read.csv("dbh.csv")
dbh$BA <- 0.005454*(dbh$DBH_AV^2) 

dbh$DIV[dbh$SPP == "PIME"] <- "c"
dbh$DIV[dbh$SPP == "BENE"] <- "d"
dbh$DIV[dbh$SPP == "POTR"] <- "d"
dbh$DIV[dbh$SPP == "SALIX"] <- "d"
write.csv(dbh, "dbh.csv", row.names = F)

dbh <- read.csv("dbh.csv")

# summing according to conif/decid per site
div_ba <- dbh %>%
  group_by(dbh$SITE, dbh$TREAT, dbh$PLOT, dbh$DIV) %>%
  summarise(BA_DIV = sum(BA)) %>%
  complete(dbh$DIV, fill = list(BA_DIV = 0)) %>%
  rename(SITE = 'dbh$SITE', TREAT = 'dbh$TREAT', 
         PLOT = 'dbh$PLOT', DIV = 'dbh$DIV')

# 1 quadrant # 33_1, 18_1, 4_2, 19_2, 28_1, 44_0, 1_0, 31_0, 6_0, 9_0
unique(dbh$PLOT[dbh$QUAD ==1])
div_ba$BA_DIV[div_ba$PLOT == "33_1"] <- div_ba$BA_DIV[div_ba$PLOT == "33_1"]*2
div_ba$BA_DIV[div_ba$PLOT == "18_1"] <- div_ba$BA_DIV[div_ba$PLOT == "18_1"]*2
div_ba$BA_DIV[div_ba$PLOT == "4_2"] <- div_ba$BA_DIV[div_ba$PLOT == "4_2"]*2
div_ba$BA_DIV[div_ba$PLOT == "19_2"] <- div_ba$BA_DIV[div_ba$PLOT == "19_2"]*2
div_ba$BA_DIV[div_ba$PLOT == "28_1"] <- div_ba$BA_DIV[div_ba$PLOT == "28_1"]*2
div_ba$BA_DIV[div_ba$PLOT == "44_0"] <- div_ba$BA_DIV[div_ba$PLOT == "44_0"]*2
div_ba$BA_DIV[div_ba$PLOT == "1_0"] <- div_ba$BA_DIV[div_ba$PLOT == "1_0"]*2
div_ba$BA_DIV[div_ba$PLOT == "31_0"] <- div_ba$BA_DIV[div_ba$PLOT == "31_0"]*2
div_ba$BA_DIV[div_ba$PLOT == "6_0"] <- div_ba$BA_DIV[div_ba$PLOT == "6_0"]*2
div_ba$BA_DIV[div_ba$PLOT == "9_0"] <- div_ba$BA_DIV[div_ba$PLOT == "9_0"]*2

# 0.1 quadrant # 15, 12, 50, 64
unique(dbh$PLOT[dbh$QUAD == 0.1])
div_ba$BA_DIV[div_ba$PLOT == "15_3"] <- div_ba$BA_DIV[div_ba$PLOT == "15_3"]*20
div_ba$BA_DIV[div_ba$PLOT == "12_1"] <- div_ba$BA_DIV[div_ba$PLOT == "12_1"]*20
div_ba$BA_DIV[div_ba$PLOT == "50_1"] <- div_ba$BA_DIV[div_ba$PLOT == "50_1"]*20
div_ba$BA_DIV[div_ba$PLOT == "64_1"] <- div_ba$BA_DIV[div_ba$PLOT == "64_1"]*20

ggplot(div_ba, aes(x = as.factor(TREAT), y = BA_DIV)) + geom_boxplot()

write.csv(div_ba, here("tree regen manuscript/ba.csv"), row.names = F)

ba <- read.csv(here("tree regen manuscript/ba.csv"))
div_ba <- ba %>%
  pivot_wider(names_from = DIV, values_from = BA_DIV)  %>%
  rename(CONIF_BA = c, DECID_BA = d)
write.csv(div_ba, here("tree regen manuscript/ba.csv"), row.names = F)

# calculating according to spp to allow for modeling
dbh <- read.csv("dbh.csv")
spp_ba <- dbh %>%
  group_by(dbh$SITE, dbh$TREAT, dbh$PLOT, dbh$SPP) %>%
  summarise(BA_DIV = sum(BA)) %>%
  complete(dbh$SPP, fill = list(BA_DIV = 0)) %>%
  rename(SITE = 'dbh$SITE', TREAT = 'dbh$TREAT', 
         PLOT = 'dbh$PLOT', SPP = 'dbh$SPP')
spp_ba$DIV[spp_ba$SPP == "PIME"] <- "c"
spp_ba$DIV[spp_ba$SPP == "BENE"] <- "d"
spp_ba$DIV[spp_ba$SPP == "POTR"] <- "d"
spp_ba$DIV[spp_ba$SPP == "SALIX"] <- "d"
write.csv(spp_ba, here("tree regen manuscript/spp_ba.csv"), row.names = F)
