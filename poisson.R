#  poisson distribution 
library(sjPlot) 
library(here)
library(parameters)
library(stats)
library(ggplot2) 
library(cowplot); theme_set(theme_cowplot()) 
options(scipen = 9999)

# bringing in data
  # density
  all <- read.csv(here("species_dens.csv")); all <- all[all$TREAT !=0,] # removing unburned sites
  # basal area
  ba <- read.csv(here("ba.csv"), stringsAsFactors = F); ba <- ba[ba$TREAT != 0,]

# conifer density
  CONIF_dens <- species_dens %>%
    filter(DIV == "c")
  # all$CONIF_COUNT_HA_log <- log(all$CONIF_COUNT_HA + 1)
  mC_D <- glm(COUNT_log ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = CONIF_dens) 
  # residuals
    plot(mC_D)
  # parameters
    summary(mC_D); model_parameters(mC_D)
  # plots
    plot_model(mC_D, show.values = T, show.p = T) # effect sizes
    plot_model(mC_D, type = "pred", terms = c("TREAT", "SITE")) # predicted values
  # testing quasi poisson
    CONIF_dens$COUNT_log <- log(CONIF_dens$COUNT + 1)
    mC_D_qp <- glm(COUNT_log ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson, data = CONIF_dens) 
    # residuals
    plot(mC_D_qp)
    # parameters
    summary(mC_D_qp); model_parameters(mC_D_qp)

# deciduous density
    DECID_dens <- species_dens %>%
      filter(DIV == "d")
    DECID_dens$COUNT_log <- log(DECID_dens$COUNT + 1)
  mD_D <- glm(COUNT_log ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = DECID_dens) 
  # residuals
   plot(mD_D)
  # parameters
   summary(mD_D); model_parameters(mD_D)
  # plots 
    plot_model(mD_D, show.values = T, show.p = T) # effect sizes
    plot_model(mD_D, type = "pred", terms = c("TREAT", "SITE")) # predicted values

# TEST deciduous density
    mD_D <- glm(COUNT_log ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson, data = DECID_dens)
    # residuals
    plot(mD_D)
    # parameters
    summary(mD_D); model_parameters(mD_D)
    # plots 
    plot_model(mD_D, show.values = T, show.p = T) # effect sizes
    plot_model(mD_D, type = "pred", terms = c("TREAT", "SITE"))    
    
# conifer basal area
    CONIF_BA <- ba %>%
      filter(DIV == "c")
    mC_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = CONIF_BA) 
  # residuals
    plot(mC_BA)
  # parameters
    summary(mC_BA); model_parameters(mC_BA)
  # plots
    plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
# quasi
    mC_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson, data = CONIF_BA) 
    # residuals
    plot(mC_BA)
    # parameters
    summary(mC_BA); model_parameters(mC_BA)
    # plots
    plot_model(mC_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mC_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
    
#deciduous basal area
    DECID_BA <- ba %>%
      filter(DIV == "d")
mD_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = poisson(link = "log"), data = DECID_BA) 
  # residuals
    plot(mD_BA)
  # parameters
    summary(mD_BA); model_parameters(mD_BA)
  # plots
    plot_model(mD_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
    
# quasi 
    mD_BA <- glm(BA ~ TREAT + SITE + (TREAT*SITE), family = quasipoisson, data = ba) 
    # residuals
    plot(mD_BA)
    # parameters
    summary(mD_BA); model_parameters(mD_BA)
    # plots
    plot_model(mD_BA, show.values = T, show.p = T, sort.est = T) # effect sizes
    plot_model(mD_BA, type = "pred", terms = c("TREAT", "SITE")) # predicted values
