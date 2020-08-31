# basal area
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)
se <- function(x) sqrt(var(x)/length(x))

dalton <- read.csv(here("data/Dalton_DBH.csv"), stringsAsFactors = F)

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
  
# adding expansion factor  
  dalton$QUAD[dalton$QUAD == 2] <- 50	#assuming a plot is 1/25 of a ha, so half a plot is 1/50 of a ha.
  dalton$QUAD[dalton$QUAD == 1] <- 100
  dalton$QUAD[dalton$QUAD == .2] <- 500
  
# calculating ba
    dalton$BA_unscale <- (pi * (dalton$DBH/2)^2)/10000 	#convert to ba and cm2 to m2. Just pi*r2
    dalton$BA_ha <- dalton$BA_unscale* dalton$QUAD		#multiply by expansion factor 
    
  # # scaling ba
  #   dalton$BA <- dalton$BA_unscale
  # 
  #   unique(dalton$PLOT[dalton$QUAD ==1]) # 44_0 28_1
  #   dalton$BA[dalton$PLOT == "28_1"] <- dalton$BA[dalton$PLOT == "28_1"]*2
  #   dalton$BA[dalton$PLOT == "44_0"] <- dalton$BA[dalton$PLOT == "44_0"]*2
  # 
  #   unique(dalton$PLOT[dalton$QUAD == 0.2]) # 15_3
  #   dalton$BA[dalton$PLOT == "15_3"] <- dalton$BA[dalton$PLOT == "15_3"]*10
  #   
  #   unique(dalton$PLOT[dalton$QUAD == 0.1]) # 15_3 12_1 50_1 
  #   dalton$BA[dalton$PLOT == "12_1"] <- dalton$BA[dalton$PLOT == "12_1"]*20
  #   dalton$BA[dalton$PLOT == "50_1"] <- dalton$BA[dalton$PLOT == "50_1"]*20

    # # summing according to species
     dalton_ba <- dalton %>%
       group_by(SITE, TREAT, PLOT, DIV, SPP) %>%
       summarise(BA_SUM = sum(BA_ha))

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

# calculating ba
    # adding expansion factor  
    steese$QUAD[steese$QUAD == 2] <- 50	#assuming a plot is 1/25 of a ha, so half a plot is 1/50 of a ha.
    steese$QUAD[steese$QUAD == 1] <- 100

    # calculating ba
    steese$BA_unscale <- (pi * (steese$DBH/2)^2)/10000 	#convert to ba and cm2 to m2. Just pi*r2
    steese$BA_ha <- steese$BA_unscale* steese$QUAD
    
    # # scaling ba
    # steese$BA <- steese$BA_unscale
    # 
    # unique(steese$PLOT[steese$QUAD ==1]) # "33_1" "18_1" "4_2"  "19_2" "28_1" "1_0"  "31_0" "6_0"  "9_0" 
    # steese$BA[steese$PLOT == "33_1"] <- steese$BA[steese$PLOT == "33_1"]*2
    # steese$BA[steese$PLOT == "18_1"] <- steese$BA[steese$PLOT == "18_1"]*2
    # steese$BA[steese$PLOT == "4_2"] <- steese$BA[steese$PLOT == "4_2"]*2
    # steese$BA[steese$PLOT == "19_2"] <- steese$BA[steese$PLOT == "19_2"]*2
    # steese$BA[steese$PLOT == "28_1"] <- steese$BA[steese$PLOT == "28_1"]*2
    # steese$BA[steese$PLOT == "6_0"] <- steese$BA[steese$PLOT == "6_0"]*2
    # steese$BA[steese$PLOT == "1_0"] <- steese$BA[steese$PLOT == "1_0"]*2
    # steese$BA[steese$PLOT == "31_0"] <- steese$BA[steese$PLOT == "31_0"]*2
    # steese$BA[steese$PLOT == "9_0"] <- steese$BA[steese$PLOT == "9_0"]*2
    # 
     # summing according to species
     steese_ba <- steese %>%
       group_by(SITE, TREAT, PLOT, DIV, SPP) %>%
       summarise(BA_SUM = sum(BA_ha))

 species_ba <- rbind(dalton_ba, steese_ba)
 write.csv(species_ba, "species_ba.csv", row.names = F)


# also writing full data file for modeling 
ba <- rbind(dalton, steese)
write.csv(ba, "ba.csv", row.names = F)

#############################################
# getting values 
species_ba <- read_csv(here("species_ba.csv"))

treat_ba <- species_ba %>%
  group_by(SITE, TREAT, DIV) %>%
  summarise(AV = mean(BA_SUM), SE = se(BA_SUM))

treat_ba$SE <- round(treat_ba$SE, digits = 1)
treat_ba$AV <- round(treat_ba$AV, digits = 1)

dalt_con_ba <- treat_ba %>%
  filter(SITE == "DALTON") %>%
  filter(DIV == "c")

stee_con_ba <- treat_ba %>%
  filter(SITE == "STEESE") %>%
  filter(DIV == "c")

dalt_dec_ba <- treat_ba %>%
  filter(SITE == "DALTON") %>%
  filter(DIV == "d")

stee_dec_ba <- treat_ba %>%
  filter(SITE == "STEESE") %>%
  filter(DIV == "d")
