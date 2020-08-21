#  testing distributions: basal area
library(tidyverse)
library(sjPlot)
library(MASS)
library(here)
# library(parameters)
# library(stats)
library(cowplot); theme_set(theme_cowplot()) 
# options(scipen = 9999)

# bringing in data
ba <- read.csv(here("ba.csv"), stringsAsFactors = F)

  # removing unburned sites
  ba <- ba[ba$TREAT != 0,]
  
  # looking at format of data, first three rows
  ba[1:3,]
  ba$BA_ha <- ba$BA * 50

#####################################################   
## conifer basal area
##
CONIF_BA <- ba %>%
    filter(DIV == "c") %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(BA = sum(BA_ha)) %>%
    ungroup() %>%
    complete(SITE, TREAT, PLOT, fill = list(BA = 0))
 hist(CONIF_BA$BA)
 CONIF_BA[CONIF_BA$BA > 100,] # checking high value
  
    mC_BA.lm <- lm(BA ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
    mC_BA.log <- lm(log(BA + 1) ~ TREAT + SITE + (TREAT*SITE), data = CONIF_BA) 
    mC_BA.ga <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = gaussian(link = "identity"), data = CONIF_BA)
    
    # residuals
    par(mfrow=c(3,4))
    plot(mC_BA.lm, main = "Linear")
    plot(mC_BA.log, main = "logged")
    plot(mC_BA.ga, main = "Gaussian")
    
    # parameters
    summary(mC_BA.lm); #model_parameters(mC_BA)
    summary(mC_BA.log)
    summary(mC_BA.ga)
    
#################################################    
## deciduous basal area
##     
    DECID_BA <- ba %>%
      filter(DIV == "d") %>%
      group_by(SITE, TREAT, PLOT) %>%
      summarise(BA = sum(BA_ha)) %>%
      ungroup() %>%
      complete(SITE, TREAT, PLOT, fill = list(BA = 0))
    hist(DECID_BA$BA)
  
  mD_BA.lm <- lm(BA  ~ TREAT + SITE + (TREAT*SITE), data = DECID_BA)
  mD_BA.log <- lm(log(BA+1)  ~ TREAT + SITE + (TREAT*SITE), data = DECID_BA)
  mD_BA.ga <- glm(BA  ~ TREAT + SITE + (TREAT*SITE), family = gaussian(link = "identity"), data = DECID_BA)
  
  par(mfrow=c(3,4))
  plot(mD_BA.lm, main="Linear")
  plot(mD_BA.log, main= "Logged")
  plot(mD_BA.ga, main="Guassian")
  
  # parameters
  summary(mD_BA.lm); # model_parameters(mD_BA)
  summary(mD_BA.log)
  summary(mD_BA.ga)
