# interaction model
library(sjPlot) 
library(here)
library(parameters)
library(olsrr)
library(ggplot2) 
library(cowplot); theme_set(theme_cowplot()) 
options(scipen = 9999)

# bringing in data
  # density
    all <- read.csv(here("all.csv")); all <- all[all$TREAT !=0,] # removing unburned sites
  # basal area
    ba <- read.csv(here("ba.csv"), stringsAsFactors = F); ba <- ba[ba$TREAT != 0,]

# conifer density
  all$CONIF_COUNT_HA_log <- log(all$CONIF_COUNT_HA + 1)
  mC_D <- lm(CONIF_COUNT_HA_log ~ TREAT + SITE + TREAT*SITE, data = all) 
  # residuals
    ols_plot_resid_qq(mC_D)
  # parameters
    summary(mC_D); model_parameters(mC_D)
  # plots
    plot_model(mC_D, show.values = T, show.p = T) # effect sizes
    plot_model(mC_D, type = "pred", terms = c("TREAT", "SITE")) # predicted values

# deciduous density
  all$DECID_COUNT_HA_log <- log(all$DECID_COUNT_HA + 1) # log
  mD_D <- lm(DECID_COUNT_HA_log ~ TREAT + TREAT*SITE, data = all) 
  # residuals
    ols_plot_resid_qq(mD_D)
  # parameters
    summary(mD_D); model_parameters(mD_D)
  # plots 
    plot_model(mD_D, show.values = T, show.p = T) # effect sizes
    plot_model(mD_D, type = "pred", terms = c("TREAT", "SITE")) # predicted values
  
# conifer basal area
  ba$CONIF_BA_log <- log(ba$CONIF_BA + 1)  
  mC_BA <- lm(CONIF_BA_log ~ TREAT + TREAT*SITE, data = ba)
  # residuals
    ols_plot_resid_qq(mC_BA)
  # parameters
    summary(mC_BA); model_parameters(mC_BA)
  # plots
    plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values

#deciduous basal area
  ba$DECID_BA_log <- log(ba$DECID_BA + 1) # log
  mD_BA <- lm(DECID_BA_log ~ TREAT + SITE + TREAT*SITE, data = ba)
  # residuals
    ols_plot_resid_qq(mD_BA)
  # parameters
    summary(mD_BA); model_parameters(mD_BA)
  # plots
    plot_model(mD_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
  