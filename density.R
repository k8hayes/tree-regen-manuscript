# density
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

dalton <- read.csv(here("data/dalton_DBH.csv"))

  # merging salix
  dalton$SPP[dalton$SPP == "SAGL"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_3"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_4"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_5"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_6"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_7"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_8"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SA_?"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SADE"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SAPU"] <- "SALIX"
  dalton$SPP[dalton$SPP == "SAGL_R"] <- "SALIX"

  # dropping unknown
    dalton <- dalton %>%
      filter(SPP != "UNKNOWN")

  # dropping dead trees
    dalton <- dalton %>%
      filter(CANOPY > 0)

  # adding conifer and deciduous divisons
    dalton$DIV[dalton$SPP == "PIME"] <- "c"
    dalton$DIV[dalton$SPP == "BENE"] <- "d"
    dalton$DIV[dalton$SPP == "POTR"] <- "d"
    dalton$DIV[dalton$SPP == "SALIX"] <- "d"
    dalton$DIV[dalton$SPP == "ARCTO"] <- "d"
    dalton$DIV[dalton$SPP == "ALCR"] <- "d"
    
    # summing according to species
    dalton_dens <- dalton %>%
      group_by(SITE, TREAT, PLOT, DIV, SPP) %>%
      tally() %>%
      rename("COUNT_unscale" = "n")

    dalton_dens$COUNT <- dalton_dens$COUNT_unscale
    unique(dalton$PLOT[dalton$QUAD ==1]) # 44_0 28_1
    dalton_dens$COUNT[dalton_dens$PLOT == "28_1"] <- dalton_dens$COUNT[dalton_dens$PLOT == "28_1"]*2
    dalton_dens$COUNT[dalton_dens$PLOT == "44_0"] <- dalton_dens$COUNT[dalton_dens$PLOT == "44_0"]*2
    
    unique(dalton$PLOT[dalton$QUAD == 0.1]) # 15_3 12_1 50_1 
    dalton_dens$COUNT[dalton_dens$PLOT == "15_3"] <- dalton_dens$COUNT[dalton_dens$PLOT == "15_3"]*20
    dalton_dens$COUNT[dalton_dens$PLOT == "12_1"] <- dalton_dens$COUNT[dalton_dens$PLOT == "12_1"]*20
    dalton_dens$COUNT[dalton_dens$PLOT == "50_1"] <- dalton_dens$COUNT[dalton_dens$PLOT == "50_1"]*20

    dalton_dens <- dalton_dens %>%
      select(SITE, PLOT, TREAT, DIV, SPP, COUNT)
  
steese <- read.csv(here("data/Steese_DBH.csv"), stringsAsFactors = F)   
    
    # merging salix
    steese$SPP[steese$SPP == "SAGL"] <- "SALIX"
    steese$SPP[steese$SPP == "SA_4"] <- "SALIX"
    steese$SPP[steese$SPP == "SA_5"] <- "SALIX"
    steese$SPP[steese$SPP == "SA_6"] <- "SALIX"
    steese$SPP[steese$SPP == "SA_7"] <- "SALIX"
    steese$SPP[steese$SPP == "SADE"] <- "SALIX"
    
    # adding conifer and deciduous divisons
    steese$DIV[steese$SPP == "PIME"] <- "c"
    steese$DIV[steese$SPP == "BENE"] <- "d"
    steese$DIV[steese$SPP == "POTR"] <- "d"
    steese$DIV[steese$SPP == "POBA"] <- "d"
    steese$DIV[steese$SPP == "SALIX"] <- "d"
    steese$DIV[steese$SPP == "ARCTO"] <- "d"
    steese$DIV[steese$SPP == "ALCR"] <- "d"
    
    # dropping dead trees
    steese <- steese %>%
      filter(CANOPY > 0)
    
    # summing according to species
    steese_dens <- steese %>%
      group_by(SITE, TREAT, PLOT, DIV, SPP) %>%
      tally() %>%
      rename("COUNT_unscale" = "n")
    
    # scaling up
    steese_dens$COUNT <- steese_dens$COUNT_unscale
    unique(steese$PLOT[steese$QUAD ==1]) # "33_1" "18_1" "4_2"  "19_2" "28_1" "1_0"  "31_0" "6_0"  "9_0" 
    steese_dens$COUNT[steese_dens$PLOT == "33_1"] <- steese_dens$COUNT[steese_dens$PLOT == "33_1"]*2
    steese_dens$COUNT[steese_dens$PLOT == "18_1"] <- steese_dens$COUNT[steese_dens$PLOT == "18_1"]*2
    steese_dens$COUNT[steese_dens$PLOT == "4_2"] <- steese_dens$COUNT[steese_dens$PLOT == "4_2"]*2
    steese_dens$COUNT[steese_dens$PLOT == "19_2"] <- steese_dens$COUNT[steese_dens$PLOT == "19_2"]*2
    steese_dens$COUNT[steese_dens$PLOT == "28_1"] <- steese_dens$COUNT[steese_dens$PLOT == "28_1"]*2
    steese_dens$COUNT[steese_dens$PLOT == "6_0"] <- steese_dens$COUNT[steese_dens$PLOT == "6_0"]*2
    steese_dens$COUNT[steese_dens$PLOT == "1_0"] <- steese_dens$COUNT[steese_dens$PLOT == "1_0"]*2
    steese_dens$COUNT[steese_dens$PLOT == "31_0"] <- steese_dens$COUNT[steese_dens$PLOT == "31_0"]*2
    steese_dens$COUNT[steese_dens$PLOT == "9_0"] <- steese_dens$COUNT[steese_dens$PLOT == "9_0"]*2
    
    steese_dens <- steese_dens %>%
      select(SITE, PLOT, TREAT, DIV, SPP, COUNT)
       
seedling <- read.csv(here("seedling_Density.csv"))
  
  seedling$COUNT <- seedling$SPP_COUNT_PLOT*2
  
  seedling <- seedling %>%
    select(SITE, PLOT, TREAT, DIVISION, SPP, COUNT) %>%
    rename("DIV" = "DIVISION")

dalt_seed_dens <- bind_rows(dalton_dens, steese_dens, seedling)

species_dens <- dalt_seed_dens %>%
  group_by(SITE, TREAT, PLOT, DIV, SPP) %>%
  summarise(COUNT = sum(COUNT))
species_dens$COUNT_HA <- species_dens$COUNT * 50
write.csv(species_dens,"species_dens.csv", row.names = F)
